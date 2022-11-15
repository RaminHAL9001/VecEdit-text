-- | This module provides the fundamental APIs for building a text editor based on mutable
-- vectors. These APIs are designed around interactive use, so unlike the APIs in
-- "VecEdit.Vector.Editor" and "VecEdit.Vector.Editor.GapBuffer", these APIs test the
-- indicies of all arguments and throw index exceptions when indicies are out of bounds. Obviously,
-- batch operations using these APIs will be much slower than using 'GapBuffer' or 'Editor', but
-- that is OK because these APIs are designed specifically for interactive use by end-users.
module VecEdit.Text.Editor
  ( EditText(..), EditTextState, TextBuffer, editTextLineEditor,
    newEditTextState, loadStreamEditText, runEditText, evalEditText,
    liftEditLine, cursorToEnd, getLineIndex, putLineIndex,
    mapRangeFreeze, foldLinesInRange, foldLinesFromCursor,
    lineNumber, columnNumber, textPoint,
    TextBoundsLimited(..), validateBounds, validateTextPoint,
    maxLineIndex, minLineIndex, rangeAllLines,

    -- ** Debugging
    debugViewTextEditor,
  ) where

import VecEdit.Types
  ( CharBufferSize, LineBufferSize,
    TextPoint(..), LineIndex, CharIndex, toIndex, fromIndex,
    TextRange, textRangeIsForward,
    VectorIndex, Range(..), TextRange(..), textRangeStart, textRangeEnd,
    RelativeDirection(..), RelativeIndex, GaplessIndex(..),
    TextPrimOpError(..), EditTextError(..),
  )

import VecEdit.Text.Line.Break (LineBreakSymbol(NoLineBreak))
import VecEdit.Text.Internal
  ( TextLine, textLineBreakSymbol,
    emptyTextLine, textLineNull, textLineContentLength,
    streamFoldLines, emptyTextLine,
    LineEditor(liftEditLine, editLineLiftResult, pushLine, popLine, copyBuffer, clearBuffer),
    defaultEditLineLiftResult,
    EditLineState, newEditLineState, runEditLine, editLineBreakSymbol, lineBreak,
    EditLineResult(..),
    columnNumber, minCharIndex, maxCharIndex,
    IOByteStream(..), emptyTextLine,
    streamFoldLines, newEditStreamState,
  )
import VecEdit.Vector.Editor (liftEditor, currentBuffer, putElemAt, printOverIOBuffer)
import qualified VecEdit.Vector.Editor.GapBuffer as GapBuf
import VecEdit.Vector.Editor.GapBuffer
  ( GapBufferState, newGapBufferState, gapBufferEditorState,
    GapBuffer, runGapBuffer, gapBuffer3Slice, gapBuffer3SliceInRange,
  )

import Control.Applicative (Alternative(..))
import Control.Arrow (left, (&&&), (|||))
import Control.Lens (Lens', lens, use, (&), (^.), (.~), (.=))
import Control.Monad (MonadPlus(..), forM_, void, (>=>))
import Control.Monad.Cont (MonadCont(callCC), ContT, runContT)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT(..), MonadState(..), runStateT, evalStateT)

import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import Data.Vector (Vector, freeze)
import qualified Data.Vector.Mutable as MVec
import Data.Vector.Mutable (IOVector)

----------------------------------------------------------------------------------------------------

-- | This is a function type for editing text in a 'GapBuffer'. You can quickly insert lines of text
-- in @O(1)@ time, and you can move the cursor around in @O(n)@ time where @n@ is the size of the
-- cursor motion. The stateful data of an 'EditText' function contains an 'EditLineState' line
-- editor, which you can use to construct lines of text to fill into the 'EditTextState'.
--
-- Note that you can evaluate an arbitrary 'EditLine' function within in this 'EditText' function
-- context by using 'liftEditLine'.
newtype EditText tags a = EditText (ExceptT EditTextError (StateT (EditTextState tags) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (EditTextState tags))

-- | A 'TextBuffer' is really just the state data used by the 'EditText' monad.
type TextBuffer tags = EditTextState tags

-- | The mutable state of the 'EditText' function type.
data EditTextState tags
  = EditTextState
    { theEditTextLineEditor :: !(EditLineState tags)
    , theEditTextGapBuffer  :: !(GapBufferState MVec.MVector (TextLine tags))
    }

editTextLineEditor :: Lens' (EditTextState tags) (EditLineState tags)
editTextLineEditor = lens theEditTextLineEditor $ \ a b -> a{ theEditTextLineEditor = b }

editTextGapBuffer :: Lens' (EditTextState tags) (GapBufferState MVec.MVector (TextLine tags))
editTextGapBuffer = lens theEditTextGapBuffer $ \ a b -> a{ theEditTextGapBuffer = b }

instance MonadError EditTextError (EditText tags) where
  throwError = EditText . throwError
  catchError (EditText (ExceptT try)) catch =
    EditText $
    ExceptT $
    try >>= \ case
      Left TextEditUndefined -> return (Left TextEditUndefined)
      Left err -> let (EditText (ExceptT c)) = catch err in c
      Right ok -> return (Right ok)

instance MonadFail (EditText tags) where { fail = throwError . EditTextFailed . Strict.pack; }

instance Alternative (EditText tags) where
  empty = throwError TextEditUndefined
  (EditText (ExceptT ma)) <|> (EditText (ExceptT mb)) =
    EditText $ ExceptT $ ma >>= \ case { Left TextEditUndefined -> mb; a -> return a; }

instance MonadPlus (EditText tags) where { mzero = empty; mplus = (<|>); }

instance LineEditor EditText where

  -- liftEditLine = EditLine tags a -> EditText tags a
  liftEditLine f =
    use editTextLineEditor >>=
    liftIO . runEditLine f >>= \ (result, st) ->
    editTextLineEditor .= st >>
    case result of
      EditLineOK            a    -> pure a
      EditLineFail          err  -> throwError err
      EditLinePush dir line next -> pushLine dir line >> liftEditLine next
      EditLinePop  dir      next -> popLine dir >>= liftEditLine . next

  -- pushLine :: RelativeDirection -> TextLine tags -> EditText tags ()
  pushLine dir line = do
    lbrk <- case line ^. textLineBreakSymbol of
      NoLineBreak -> use $ editTextLineEditor . editLineBreakSymbol
      lbrk -> pure lbrk
    editTextLiftGB $ GapBuf.pushItem dir $ textLineBreakSymbol .~ lbrk $ line
    liftEditLine clearBuffer
    -- This implementation is different from the 'EditLine' implementation: it actually places the
    -- line in the buffer, rather than hand it off to a continuation.

  -- popLine :: RelativeDirection -> EditText tags (TextLine tags)
  popLine dir =
    editTextLiftGB $
    use (GapBuf.relativeCursor dir) >>= \ i ->
    if i <= 0 then throwError $ PopItem dir else
    GapBuf.popItem dir
    -- This implementation is different from the 'EditLine' implementation: it actually removes the
    -- line in the buffer, rather than hand it off to a continuation.

  -- editLineLiftResult :: EditLineResult editor tags a -> EditText tags a
  editLineLiftResult = defaultEditLineLiftResult

-- | Create a new 'EditText' state. Pass an initial line buffer size as the argument, remember that
-- the size of the line buffer grows dynamically so under-sizing it is OK but being closer to the
-- maximum size will result in fewer re-allocations and line copying operations.
newEditTextState :: LineBufferSize -> CharBufferSize -> IO (EditTextState tags)
newEditTextState lineBufSize charBufSize = do
  buffer <- newGapBufferState (Just emptyTextLine) (max 8 $ maybe 32 id lineBufSize)
  lineEd <- newEditLineState charBufSize
  return EditTextState
    { theEditTextLineEditor = lineEd
    , theEditTextGapBuffer = buffer
    }

-- | Evaluate a 'GapBuffer' function within an 'EditText' on the 'EditTtextState' provided by
-- 'editTextGapBuffer'.
editTextLiftGB :: GapBuffer MVec.MVector (TextLine tags) a -> EditText tags a
editTextLiftGB f =
  EditText $
  ExceptT $
  StateT $ \ st ->
  runGapBuffer f (st ^. editTextGapBuffer) >>= \ (result, ed) ->
  pure (left EditTextError $ result, editTextGapBuffer .~ ed $ st)

-- | Run an 'EditText' function on an 'EditTextState'.
runEditText
  :: EditText tags a
  -> EditTextState tags
  -> IO (Either EditTextError a, EditTextState tags)
runEditText (EditText f) = runStateT (runExceptT f)

-- | Like 'runEditText' but discards the editor when done.
evalEditText :: EditText tags a -> EditTextState tags -> IO (Either EditTextError a)
evalEditText = fmap (fmap fst) . runEditText

-- | Read an entire 'IOByteStream' (remember, a file 'Handle' is a type of @stream@) into the
-- current text buffer.
loadStreamEditText :: forall stream tags . IOByteStream stream => stream -> EditText tags ()
loadStreamEditText handle =
  liftEditLine $
  newEditStreamState () >>=
  void .
  streamFoldLines handle (const $ lineBreak Before)

----------------------------------------------------------------------------------------------------

class TextBoundsLimited index where
  -- | This function checks an @index@ type (one of 'LineIndex', 'CharIndex', or 'TextPoint'), and
  -- if it is not out of bounds, returns 'Right' with the @index@ value, otherwise returns 'Left'
  -- with an appropriate 'EditTextError' value set. The 'Right' value returned might be different
  -- from the @index@ value passed in if the @index@ is 'maxBound' or 'minBound', in which case the
  -- minimum or maximum valid @index@ value is returned.
  inBounds :: index -> EditText tags (Either EditTextError index)

minLineIndex :: EditText tags LineIndex
minLineIndex = pure 1

-- | Return the maximum 'LineIndex' value for the current 'EditTextState' of the 'EditText' function
-- context. An empty 'EditTextState' will return a value of 0.
maxLineIndex :: EditText tags LineIndex
maxLineIndex =
  toIndex . GaplessIndex . subtract 1 <$>
  editTextLiftGB GapBuf.cursorElemCount

-- | Return a 'TextRange' value that covers all lines, returns 'Nothing' if the buffer is empty.
rangeAllLines :: EditText tags (Maybe (TextRange LineIndex))
rangeAllLines = maxLineIndex >>= \ case
  0 -> pure Nothing
  n -> pure $ Just $ TextRange 1 n

genericInBounds
  :: (Monad m, Eq i, Ord i, Bounded i)
  => m i
  -> m i
  -> (i -> i -> i -> EditTextError)
  -> (i -> m (Either EditTextError i))
genericInBounds getmin getmax mkerr i =
  getmin >>= \ lo ->
  getmax >>= \ hi ->
  if i == minBound then pure (Right lo)
  else if i == maxBound then pure (Right hi)
  else if lo <= i && i <= hi then pure (Right i)
  else pure $ Left $ mkerr lo hi i

instance TextBoundsLimited LineIndex where
  inBounds = genericInBounds minLineIndex maxLineIndex $ \ lo hi i ->
    EditLineIndexError
    { editLineInvalidIndex = i
    , editLineMinBound = lo
    , editLineMaxBound = hi
    }

instance TextBoundsLimited CharIndex where
  inBounds i =
    lineNumber >>= \ line ->
    liftEditLine $
    flip (genericInBounds minCharIndex maxCharIndex) i $ \ lo hi i ->
    EditCharIndexError
    { editCharOnLine = line
    , editCharInvalidIndex = i
    , editCharMinBound = lo
    , editCharMaxBound = hi
    }

instance TextBoundsLimited TextPoint where
  inBounds pt = catchError (Right . fst <$> validateTextPoint pt) (pure . Left)

validateBounds :: TextBoundsLimited i => i -> EditText tags i
validateBounds = inBounds >=> (throwError ||| pure)

-- | Get the current 'LineIndex' for the cursor in an 'EditText' function editing a text buffer.
lineNumber :: EditText tags LineIndex
lineNumber =
  toIndex . GaplessIndex . subtract 1 <$>
  editTextLiftGB (use GapBuf.beforeCursor)

-- | Get the current cursor position in an 'EditText' function editing a text buffer.
textPoint :: EditText tags TextPoint
textPoint = TextPoint <$> lineNumber <*> liftEditLine columnNumber

-- | Check the cursor position, return the line under the cursor, throw an exception if the cursor
-- is out of bounds.
validateTextPoint :: TextPoint -> EditText tags (TextPoint, TextLine tags)
validateTextPoint i = do
  line <- inBounds (theTextPointRow i) >>= EditText . ExceptT . pure
  curLine <- lineNumber
  if line == curLine then do
      char <- inBounds (theTextPointColumn i) >>= EditText . ExceptT . pure
      textLine <- copyBuffer
      pure (TextPoint line char, textLine)
    else do
      textLine <- getLineIndex line
      let char = theTextPointColumn i
      let hi = toIndex $ GaplessIndex $ textLineContentLength textLine
      if char < hi then
          pure (TextPoint line char, textLine)
        else
          throwError $
          EditCharIndexError
          { editCharOnLine = line
          , editCharInvalidIndex = theTextPointColumn i
          , editCharMinBound = 1
          , editCharMaxBound = hi
          }

----------------------------------------------------------------------------------------------------

-- | A type of functions that can be used to fold over line elements of an 'EditTextState'
-- buffer. It instantiates 'StateT' so that you can modify the @fold@ value using
-- 'Control.Monad.State.modify', 'Control.Monad.State.get', and 'Control.Monad.State.put'. It also
-- instantiates 'MonadCont', and the APIs in this module (such as 'foldLinesFromCursor') will
-- usually pass a halting function as an argument to the iterator continuation so that the iterator
-- can choose to halt the loop.
--
-- One important property of functions of this type is that you can fold over the 'TextLines' in a
-- 'TextBuffer' without changing the cursor position. In order to ensure this property holds,
-- functions of this type are allowed to modify 'TextLine's in the 'TextBuffer'__in place__ so long
-- as the number of 'TextLine's in the 'TextBuffer' does not change. So you can change anything
-- about the line of text as long as you do not delete the 'LineBreakSymbol' at the end of the
-- 'TextLine', which would change the number of lines in the buffer. You can even change
-- 'textLineBreakSymbol' to a different non-'NoLineBreak' line breaking symbol, but if you set it to
-- 'NoLineBreak', 'theEditTextLineBreak' is silently inserted automatically before being placed into
-- the 'TextBuffer'.
newtype FoldTextLines fold a
  = FoldTextLines (ContT fold (StateT fold IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState fold, MonadCont)

-- | This is the type of the continuation function taken by the 'foldLinesFromCursor' and
-- 'foldLinesInRange' functions. When you write functions of this type, you can also use the
-- 'MonadState' instance functions to 'get', 'modify', and 'put' the @fold@ value.
--
-- When you write a functions of this type, you may return a value of type @'Maybe ('TextLine'
-- tags)@. If you return 'Nothing', then the 'TextLine' is not modified. If you return a 'Just'
-- value containing an updated 'TextLine', the 'TextLine' is written back to the buffer in place. If
-- 'textLineBreakSymbol' is 'NoLineBreak', the previous line break value (the value of
-- 'textLineBreakSymbol' for the 3rd argument to this function) is used to ensure the number of lines
-- is not modified, which is one of the properties of the 'FoldTextLines' function type.
type FoldLinesStep tags fold void
  = (fold -> FoldTextLines fold void)
     -- ^ A fold operation loops over all lines, evaluate this function to break out of the loop and
     -- return a @fold@ value immeidiately.
  -> LineIndex
     -- ^ Tells you what line you are on.
  -> TextLine tags
     -- ^ The content of the line.
  -> FoldTextLines fold (Maybe (TextLine tags))

-- | This function evaluates a function of type 'FoldTextLines' using an initial @fold@ value. This
-- function does no folding or looping on it's own, but can be used to construct loop functions that
-- perform folds over the @fold@ state value.
runFoldTextLines :: FoldTextLines fold a -> fold -> IO fold
runFoldTextLines (FoldTextLines f) = evalStateT (runContT f (const get))

-- | Evaluate a 'FoldTextLines' function over a slice of the 'TextBufferState'
foldTextLinesOverSlice
  :: (fold -> FoldTextLines fold void) -- ^ the halt function
  -> Bool -- ^ fold in a forward or reverse direction over the vector
  -> (VectorIndex -> LineIndex)
  -> FoldLinesStep tags fold void -- ^ the fold function
  -> IOVector (TextLine tags)
  -> FoldTextLines fold ()
foldTextLinesOverSlice halt forward index f vec =
  let len = MVec.length vec in
  if len <= 0 then return () else
  let vecIndicies = (if forward then id else ((len - 1) -)) <$> [0 .. len - 1] in
  forM_ vecIndicies $ \ i ->
  liftIO (MVec.read vec i) >>= \ line ->
  f halt (index i) line >>=
  foldTextLineMaybeUpdate (MVec.write vec i) line

-- | This is an internal function used to inspect the return value of a 'FoldLinesStep' function. If
-- the 'TextLine' returned is non-'Nothing', overwrite the previous line index in the buffer with
-- the given value, while also performing the 'textLineMustBreak' test.
foldTextLineMaybeUpdate
  :: (TextLine tags -> IO ())
  -> TextLine tags
  -> Maybe (TextLine tags)
  -> FoldTextLines fold ()
foldTextLineMaybeUpdate writeBack currentLine =
  maybe (pure ()) $
  liftIO . writeBack .
  textLineMustBreak (currentLine ^. textLineBreakSymbol)

-- | An internal function used by 'FoldTextLines'. Whenever a continuation function can update one
-- of the 'TextLine's in a buffer, the updated line must be checked to make sure it does not set
-- 'textLineBreakSymbol' to a 'NoLineBreak' value. This function performs that check, and changes a
-- 'NoLineBreak' symbol with the provided 'LineBreakSymbol', which should either be
-- 'theEditLineTextBreak', or 'textLineBreakSymbol' for the line that was there before.
textLineMustBreak :: LineBreakSymbol -> TextLine tags -> TextLine tags
textLineMustBreak defaultSymbol line =
  line & case line ^. textLineBreakSymbol of
    NoLineBreak -> textLineBreakSymbol .~ defaultSymbol
    _           -> id

----------------------------------------------------------------------------------------------------

-- | Get the 'TextLine' at a particular 'LineIndex'.
getLineIndex :: LineIndex -> EditText tags (TextLine tags)
getLineIndex = editTextLiftGB . GapBuf.getGaplessIndex . fromIndex

-- | Replace the 'TextLine' at the given 'LineIndex' with the given 'TextLine'.
putLineIndex :: LineIndex -> TextLine tags -> EditText tags ()
putLineIndex i line =
  editTextLiftGB $ do
    vecIndex <- GapBuf.fromGaplessIndex $ fromIndex i
    liftEditor $ putElemAt vecIndex line

-- | Move the cursor to either end of the buffer. 'Before' moves the cursor to the first of the
-- buffer, 'After' moves the cursor to the last line of the buffer.
cursorToEnd :: RelativeDirection -> EditText tags RelativeIndex
cursorToEnd dir =
  editTextLiftGB $
  use (GapBuf.relativeCursor dir) >>= \ count ->
  GapBuf.shiftCursor $ case dir of
    Before -> negate count
    After  -> count

----------------------------------------------------------------------------------------------------

-- | This function starts at the current cursor and moves in the direction 'Before' or 'After' the
-- cursor iterating over each line, passing each line to the folding function along with it's line
-- number, until the cursor has come to the beginning or end of the 'EditTextState' buffer has been
-- reached. The buffer itself is left unmodified, and the cursor will remain in the position it
-- started before this function was called. This evaluates 'flushLine'.
foldLinesFromCursor
  :: Eq (TextLine tags)
  => RelativeDirection
  -> FoldLinesStep tags fold void
  -> fold
  -> EditText tags fold
foldLinesFromCursor dir f fold =
  case dir of
    Before ->
      editTextLiftGB gapBuffer3Slice >>= \ (lo, _gap, _hi) ->
      liftIO $
      runFoldTextLines
      ( callCC $ \ halt ->
        let loop i0 =
              if i0 > 0 then
                let i = i0 - 1 in seq i $!
                liftIO (MVec.read lo i) >>= \ line ->
                f halt (toIndex $ GaplessIndex i) line >>=
                foldTextLineMaybeUpdate (MVec.write lo i) line >>
                loop i
              else get
        in
        loop (MVec.length lo)
      )
      fold
    After ->
      ( editTextLiftGB $ do
         (_lo, _gap, hi) <- gapBuffer3Slice
         offset <- use GapBuf.beforeCursor
         pure (hi, offset)
      ) >>= \ (hi, offset) ->
      liftIO $
      runFoldTextLines
      ( callCC $ \ halt ->
        let loop i =
              if i >= MVec.length hi then get else
              liftIO (MVec.read hi i) >>= \ line ->
              f halt (toIndex $ GaplessIndex $ offset + i) line >>=
              foldTextLineMaybeUpdate (MVec.write hi i) line >>
              (loop $! i + 1)
        in
        loop 0
      )
      fold

-- | This function takes a 'TextRange' and then folds over every 'TextLine' in that range using the
-- given 'FoldTextLines' function. This evaluates 'flushLine'.
foldLinesInRange
  :: Eq (TextLine tags)
  => TextRange LineIndex
  -> FoldLinesStep tags fold void
  -> fold
  -> EditText tags fold
foldLinesInRange range f fold = do
  let forward = textRangeIsForward range
  (lo, hi) <- editTextLiftGB $ do
    let start = fromIndex (range ^. textRangeStart)
    let end   = fromIndex (range ^. textRangeEnd)
    loIndex <- GapBuf.fromGaplessIndex $ min start end
    hiIndex <- GapBuf.fromGaplessIndex $ max start end
    (loSlice, _, hiSlice) <- gapBuffer3SliceInRange $
      Range
      { theRangeStart = loIndex
      , theRangeLength = hiIndex - loIndex + 1
      }
    let mkIndex offset = toIndex . GaplessIndex . (+ offset)
    let hiOffset = loIndex + MVec.length loSlice
    pure
      ( \ halt ->
        foldTextLinesOverSlice halt forward (mkIndex loIndex) f loSlice
      , \ halt ->
        foldTextLinesOverSlice halt forward (mkIndex hiOffset) f hiSlice
      )
  liftIO $
    flip runFoldTextLines fold $
    callCC $ \ halt ->
    (if forward then lo halt >> hi halt else hi halt >> lo halt) >>
    get

-- | Perform a map over the lines in a 'TextRange' and freeze the results in an immutable
-- buffer. The mapping function provided can simply be @('const' 'pure')@ to copy each line without
-- modification. This evaluates 'flushLine'.
mapRangeFreeze
  :: Eq tags
  => TextRange LineIndex
  -> (LineIndex -> TextLine tags -> IO a)
  -> EditText tags (Vector a)
mapRangeFreeze range f = do
  let forward = textRangeIsForward range
  editTextLiftGB $ do
    let start = fromIndex (range ^. textRangeStart)
    let end   = fromIndex (range ^. textRangeEnd)
    loIndex <- GapBuf.fromGaplessIndex $ min start end
    hiIndex <- GapBuf.fromGaplessIndex $ max start end
    (loSlice, _, hiSlice) <- gapBuffer3SliceInRange $
      Range
      { theRangeStart = loIndex
      , theRangeLength = hiIndex - loIndex + 1
      }
    let mkIndex offset = toIndex . GaplessIndex . (+ offset)
    let hiOffset = loIndex + MVec.length loSlice
    let totalLength = MVec.length loSlice + MVec.length hiSlice
    newVec <- liftIO $ MVec.new totalLength
    let mapping globalOffset localOffset slice =
          let top = MVec.length slice - 1 in
          let indicies = (if forward then id else (top -)) <$> [0 .. top] in
          flip evalStateT localOffset $
          forM_ indicies $ \ sourceIndex ->
          state (id &&& (+ 1)) >>= \ targetIndex ->
          liftIO $
          MVec.read slice sourceIndex >>=
          f (mkIndex globalOffset sourceIndex) >>=
          MVec.write newVec targetIndex
    if forward then
      mapping loIndex 0 loSlice >>
      mapping hiOffset (MVec.length loSlice) hiSlice
     else
      mapping hiOffset 0 hiSlice >>
      mapping loIndex (MVec.length hiSlice) loSlice
    liftIO $ freeze newVec

----------------------------------------------------------------------------------------------------

-- | Uses 'printOverIOBuffer' to visualize the content of the gap buffer underlying the 'EditText'
-- state, which will dump a textual visualization of the vector to @stdout@. This allows you to see
-- not just what the visible state of the text editor is, but also which elements in the buffer are
-- undefined (so you can check the consistency of the 'GapBuffer'), and to what value the @tags@ of
-- each line are set.
debugViewTextEditor :: Show tags => EditText tags ()
debugViewTextEditor =
  editTextLiftGB $
  use (gapBufferEditorState . currentBuffer) >>=
  printOverIOBuffer
  (pure . not . textLineNull)
  (\ pfx txt ->
     liftIO $
     Strict.putStr pfx >>
     print txt
  )
  (liftIO $ putStrLn "...")
