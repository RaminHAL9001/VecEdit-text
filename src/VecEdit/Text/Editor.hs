-- | This module provides the fundamental APIs for building a text editor based on mutable
-- vectors. These APIs are designed around interactive use, so unlike the APIs in
-- "VecEdit.Vector.Editor" and "VecEdit.Vector.Editor.GapBuffer", these APIs test the
-- indicies of all arguments and throw index exceptions when indicies are out of bounds. Obviously,
-- batch operations using these APIs will be much slower than using 'GapBuffer' or 'Editor', but
-- that is OK because these APIs are designed specifically for interactive use by end-users.
module VecEdit.Text.Editor
  ( EditText(..), EditTextState(..), TextBuffer,
    newEditTextState, runEditText, evalEditText,
    popLine, pushLine, cursorToEnd, getLineIndex, putLineIndex,
    flushLine, newline, insertChar, insertString,
    mapRangeFreeze, foldLinesInRange, foldLinesFromCursor, editTextLiftGapBuffer,
    lineNumber, columnNumber, textPoint,
    TextBoundsLimited(..), validateBounds, validateTextPoint,
    maxLineIndex, minLineIndex, maxCharIndex, minCharIndex,
    EditLine(..), newEditLineState, runEditLine, editLineLiftGB, runEditLineIO,
    EditLineState(..), editLineGapBuffer, editLineTextData,
    loadHandleEditText, editLineFlush,
    VecEdit.Vector.Editor.currentBuffer,

    -- ** Debugging
    debugViewTextEditor,
  ) where

import VecEdit.Types
  ( CharBufferSize, LineIndex, CharIndex, toIndex, fromIndex,
    TextPoint(..),
    TextRange, textRangeIsForward,
    VectorIndex, VectorSize, Range(..), textRangeStart, textRangeEnd,
    RelativeDirection(..), RelativeIndex, GaplessIndex(..),
    GapBufferErrorInfo(..), EditTextError(..),
  )

import VecEdit.Text.LineBreak
  ( LineBreakSymbol(NoLineBreak,LineBreakLF),
    LineBreakerState, lineBreak_LF,
  )
import VecEdit.Text.String
  ( TextLine, stringLength, textLineBreakSymbol, textLineTags,
    undefinedTextLine, textLineIsUndefined, fuseIntoTextLine,
    stringFillCharBuffer, streamReadLines, undefinedTextLine,
    IOByteStream(..), hSetByteStreamMode, undefinedTextLine,
    StringLength(..),
  )
import VecEdit.Text.TokenizerTable (TokenizerTable)
import VecEdit.Vector.Editor
  ( newEditorState, evalEditor, liftEditor,
    currentBuffer, newCurrentBuffer, putElemAt, printOverIOBuffer,
  )
import VecEdit.Vector.Editor.GapBuffer
  ( GapBufferState, newGapBufferState, gapBufferEditorState, editorToGapBufferState,
    GapBuffer, runGapBuffer, pushItem, popItem, atCursor, cursorElemCount,
    fromGaplessIndex, getGaplessIndex, relativeCursor, shiftCursor, beforeCursor, afterCursor,
    gapBuffer3Slice, gapBuffer3SliceInRange,
  )

import Control.Applicative (Alternative(..))
import Control.Arrow (left, (&&&), (|||))
import Control.Lens (Lens', lens, use, (&), (^.), (.~), (%~), (.=))
import Control.Monad (MonadPlus(..), forM_, (>=>))
import Control.Monad.Cont (MonadCont(callCC), ContT, runContT)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT(..), MonadState(..), runStateT, evalStateT)

import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import Data.Vector (Vector, freeze)
import qualified Data.Vector.Mutable as MVec
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as UMVec

import System.IO (Handle)

---- debug
--import Debug.Trace (traceM)
--import Debug.Trace (trace)
--import VecEdit.Text.String (theTextLineData)

----------------------------------------------------------------------------------------------------

-- | This is a function type for editing a single line of text with a unboxed mutable 'MVector' as
-- the character buffer. You can quickly insert characters in @O(1)@ time, and you can move the
-- cursor in @O(n)@ time where @n@ is the size of the cursor motion. Filling the buffer
newtype EditLine tags a
  = EditLine
    { unwrapEditLine :: ExceptT GapBufferErrorInfo (StateT (EditLineState tags) IO) a
    }
  deriving
  ( Functor, Applicative, Monad, MonadIO,
    MonadError GapBufferErrorInfo,
    MonadState (EditLineState tags)
  )

-- | This is the stateful data availble to you when you use functions of the 'EditLine' type.
data EditLineState tags
  = EditLineState
    { theEditLineGapBuffer  :: !(GapBufferState UMVec.MVector Char)
      -- ^ This is the mutable text that can be edited.
    , theEditLineTextData   :: !(TextLine tags)
      -- ^ If the line editor 'GapBuffer' was initialized from a 'TextLine', the 'TextLine' can be
      -- stored here. This allows the tags to be modified and copied back to a new 'TextLine'.
    , theEditLineIsModified :: !EditLineModified
    , theEditLineCursor     :: !VectorIndex
    }

instance MonadFail (EditLine tags) where { fail = EditLine . fail; }

-- | This is the mutable text that can be edited.
editLineGapBuffer :: Lens' (EditLineState tags) (GapBufferState UMVec.MVector Char)
editLineGapBuffer = lens theEditLineGapBuffer $ \ a b -> a{ theEditLineGapBuffer = b }

-- | If the line editor 'GapBuffer' was initialized from a 'TextLine', the 'TextLine' can be
-- stored here. This allows the tags to be modified and copied back to a new 'TextLine'.
editLineTextData :: Eq (TextLine tags) => Lens' (EditLineState tags) (TextLine tags)
editLineTextData =
  lens theEditLineTextData $ \ a b ->
  a{ theEditLineTextData = b
   , theEditLineIsModified =
     if theEditLineTextData a == b && b /= undefinedTextLine
      then theEditLineIsModified a
      else EditLineBufferDirty
     -- This must be set if the 'TextLine' changes.
   }

-- | Indicate whether 'theEditLineGapBuffer' needs to be flushed.
editLineIsModified :: Lens' (EditLineState tags) EditLineModified
editLineIsModified = lens theEditLineIsModified $ \ a b -> a{ theEditLineIsModified = b }

-- | This is the cursor position used when the 'EditLineBufferDirty' state is set. The cursor can
-- still be moved (kept track of by this field) but it will not shift characters around in the
-- 'LineEditState'.
editLineCursor :: Lens' (EditLineState tags) VectorIndex
editLineCursor = lens theEditLineCursor $ \ a b -> a{ theEditLineCursor = b }

newEditLineState :: CharBufferSize -> TextLine tags -> IO (EditLineState tags)
newEditLineState initSize initLine = do
  let len = stringLength initLine
  buf <- newEditorState >>= evalEditor
    (do newCurrentBuffer $
          if len >= 0 then max 512 len else
          maybe 512 (max 512) initSize
        if len >= 0 then pure 0 else
          use currentBuffer >>= \ buf ->
          liftIO $
          stringFillCharBuffer buf 0 initLine
        get
    )
  return
    EditLineState
    { theEditLineGapBuffer  = editorToGapBufferState buf
    , theEditLineTextData   = initLine
    , theEditLineIsModified = EditLineBufferDirty
    , theEditLineCursor     = 0
    }

-- | Evaluate an 'EditLine' function with a given 'EditLineState'.
runEditLineIO
  :: EditLine tags a
  -> EditLineState tags
  -> IO (Either GapBufferErrorInfo a, EditLineState tags)
runEditLineIO (EditLine (ExceptT f)) st = runStateT f st

-- | Lift a 'GapBuffer' function into the 'EditLine' function
editLineLiftGB :: GapBuffer UMVec.MVector Char a -> EditLine tags a
editLineLiftGB f = do
  (result, st) <- use editLineGapBuffer >>= liftIO . runGapBuffer f
  editLineGapBuffer .= st
  EditLine $ ExceptT $ pure result

-- | Replace 'theEditLineTextData' with the content of the current line buffer.
editLineFlush :: Eq (TextLine tags) => EditLine tags (TextLine tags)
editLineFlush =
  use editLineTextData >>= \ old ->
  use editLineIsModified >>= \ status ->
  if status == EditLineBufferLoaded then pure old else do
    line <-
      editLineLiftGB $
      fuseIntoTextLine
      (old ^. textLineBreakSymbol)
      (old ^. textLineTags)
    editLineTextData .= line
    editLineIsModified .= EditLineBufferLoaded
    pure line

----------------------------------------------------------------------------------------------------

-- | This is a function type for editing text in a 'GapBuffer'. You can quickly insert lines of text
-- in @O(1)@ time, and you can move the cursor around in @O(n)@ time where @n@ is the size of the
-- cursor motion. The stateful data of an 'EditText' function contains an 'EditLineState' line
-- editor, which you can use to construct lines of text to fill into the 'EditTextState'.
newtype EditText tags a = EditText (ExceptT EditTextError (StateT (EditTextState tags) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (EditTextState tags))

-- | A 'TextBuffer' is really just the state data used by the 'EditText' monad.
type TextBuffer tags = EditTextState tags

-- | The mutable state of the 'EditText' function type.
data EditTextState tags
  = EditTextState
    { theEditTextLineEditor     :: !(EditLineState tags)
    , theEditTextGapBuffer      :: !(GapBufferState MVec.MVector (TextLine tags))
    , theEditTextTokenizerTable :: !(TokenizerTable LineBreakSymbol LineBreakerState)
    , theEditTextLineBreak      :: !LineBreakSymbol
    }

-- | The line editor can be filled with 'TextLine's, but the characters are not copied into
-- 'theEditLineGapBuffer' until they need to be. This flag is set when the characters have been
-- copied into 'theEditLineGapBuffer' and modified, and therefore need to be flushed before cursor
-- motion occurs. If 'theEditLineGapBuffer' has not been modified or filled with the contents of
-- 'theEditLineTextData' then this value should be 'False' and it is OK to move the cursor without
-- flushing 'theEditLinGapBuffer'.
data EditLineModified
  = EditLineBufferDirty
    -- ^ The buffer is empty or out of date and needs to be filled.
  | EditLineBufferLoaded
    -- ^ The buffer is filled with the current line, but has not been
    -- modified.
  | EditLineBufferUpdated
    -- ^ The buffer is filled with the current line, and has been modified, and needs to be flushed.
  deriving (Eq, Ord, Show)

editTextLineEditor :: Lens' (EditTextState tags) (EditLineState tags)
editTextLineEditor = lens theEditTextLineEditor $ \ a b -> a{ theEditTextLineEditor = b }

editTextGapBuffer :: Lens' (EditTextState tags) (GapBufferState MVec.MVector (TextLine tags))
editTextGapBuffer = lens theEditTextGapBuffer $ \ a b -> a{ theEditTextGapBuffer = b }

-- | The default 'LineBreakSymbol' to use when breaking lines. Note that this value is never allowed
-- to be 'NoLineBreak', if you try to set it to 'NoLineBreak' it is automatically set to
-- 'LineBreakLF'.
editTextLineBreak :: Lens' (EditTextState tags) LineBreakSymbol
editTextLineBreak =
  lens theEditTextLineBreak $ \ a b ->
  a{ theEditTextLineBreak = case b of
       NoLineBreak -> LineBreakLF
       _ -> b
   }

-- | This is the 'TokenizerTable' used to determine line breaking characters. By default, it breaks
-- on '\LF' characters only, but it can also be set to break on '\CR\LF' characters.
editTextTokenizerTable
  :: Lens' (EditTextState tags) (TokenizerTable LineBreakSymbol LineBreakerState)
editTextTokenizerTable =
  lens theEditTextTokenizerTable $ \ a b -> a{ theEditTextTokenizerTable = b }

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

-- | Create a new 'EditText' state. Pass an initial line buffer size as the argument, remember that
-- the size of the line buffer grows dynamically so under-sizing it is OK but being closer to the
-- maximum size will result in fewer re-allocations and line copying operations.
newEditTextState :: VectorSize -> IO (EditTextState tags)
newEditTextState size = do
  buffer <- newGapBufferState (Just undefinedTextLine) (max 8 size)
  lineEd <- newGapBufferState Nothing 256
  return EditTextState
    { theEditTextLineEditor =
      EditLineState
      { theEditLineGapBuffer  = lineEd
      , theEditLineTextData   = undefinedTextLine
      , theEditLineIsModified = EditLineBufferDirty
      , theEditLineCursor     = 0
      }
    , theEditTextGapBuffer = buffer
    , theEditTextTokenizerTable = lineBreak_LF
    , theEditTextLineBreak = LineBreakLF
    }

-- | Evaluate a 'GapBuffer' function within an 'EditText' on the 'EditTtextState' provided by
-- 'editTextGapBuffer'.
editTextLiftGapBuffer :: GapBuffer MVec.MVector (TextLine tags) a -> EditText tags a
editTextLiftGapBuffer f =
  EditText $
  ExceptT $
  StateT $ \ st ->
  runGapBuffer f (st ^. editTextGapBuffer) >>= \ (result, ed) ->
  pure (left EditTextError $ result, editTextGapBuffer .~ ed $ st)

-- | Run an 'EditLine' function in the current 'EditText' function context.
runEditLine :: EditLine tags a -> EditText tags a
runEditLine ed = do
  (result, st) <-
    EditText (use editTextLineEditor) >>=
    liftIO . runEditLineIO ed
  EditText $ editTextLineEditor .= st
  case result of
    Left err -> throwError $ EditLineError err
    Right ok -> return ok

-- | Run an 'EditText' function on an 'EditTextState'.
runEditText
  :: EditText tags a
  -> EditTextState tags
  -> IO (Either EditTextError a, EditTextState tags)
runEditText (EditText f) = runStateT (runExceptT f)

-- | Like 'runEditText' but discards the editor when done.
evalEditText :: EditText tags a -> EditTextState tags -> IO (Either EditTextError a)
evalEditText = fmap (fmap fst) . runEditText

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
maxLineIndex = toIndex . GaplessIndex . subtract 1 <$> editTextLiftGapBuffer cursorElemCount

minCharIndex :: EditLine tags CharIndex
minCharIndex = pure 1

-- | Return the maximum 'CharIndex' value for the current 'EditLineState' of the 'EditLine' function
-- context. An empty 'EditLineState' will return a value of 0.
maxCharIndex :: EditLine tags CharIndex
maxCharIndex = toIndex . GaplessIndex . subtract 1 <$> editLineLiftGB cursorElemCount

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
  inBounds i = do
    line <- lineNumber
    runEditLine $
      genericInBounds minCharIndex maxCharIndex
      (\ lo hi i ->
       EditCharIndexError
       { editCharOnLine = line
       , editCharInvalidIndex = i
       , editCharMinBound = lo
       , editCharMaxBound = hi
       }
      ) i

instance TextBoundsLimited TextPoint where
  inBounds (TextPoint line char) =
    catchError
    ( Right <$>
      ( TextPoint <$>
        (inBounds line >>= EditText . ExceptT . pure) <*>
        (inBounds char >>= EditText . ExceptT . pure)
      )
    )
    (pure . Left)

validateBounds :: TextBoundsLimited i => i -> EditText tags i
validateBounds = inBounds >=> (throwError ||| pure)

-- | Get the current 'LineIndex' for the cursor in an 'EditText' function editing a text buffer.
lineNumber :: EditText tags LineIndex
lineNumber =
  toIndex . GaplessIndex . subtract 1 <$>
  editTextLiftGapBuffer (use beforeCursor)

-- | Get the current 'CharIndex' for the cursor in an 'EditText' function editing a line of text.
columnNumber :: EditLine tags CharIndex
columnNumber =
  toIndex . GaplessIndex . subtract 1 <$>
  editLineLiftGB (use beforeCursor)

-- | Get the current cursor position in an 'EditText' function editing a text buffer.
textPoint :: EditText tags TextPoint
textPoint = TextPoint <$> lineNumber <*> runEditLine columnNumber

-- | Check the cursor position, return the line under the cursor, throw an exception if the cursor
-- is out of bounds.
validateTextPoint :: Eq tags => TextPoint -> EditText tags (TextPoint, TextLine tags)
validateTextPoint i =
  (\ a b -> (b, a)) <$>
  runEditLine editLineFlush <*>
  (inBounds i >>= EditText . ExceptT . pure)

----------------------------------------------------------------------------------------------------

-- | Craete a new 'EditTextState' and load the content of a file 'Handle' into it. Pass an optional
-- character buffer size as the first argument, and an optional line buffer size as the second
-- argument. The 'editTextTokenizerTable' is used to perform line breaks on the input stream.
loadHandleEditText :: Maybe VectorSize -> Handle -> EditText tags ()
loadHandleEditText size handle = do
  liftIO $ hSetByteStreamMode handle
  loadStreamEditText size handle

-- | Like 'loadHandleEditText', but works on an arbitraryr 'IOByteStream' type.
loadStreamEditText :: IOByteStream stream => Maybe VectorSize -> stream -> EditText tags ()
loadStreamEditText size handle =
  -- This is a really ugly bit of non-composable code. The 'ReadLines' function type is not a monad
  -- transformer, this is done in the hopes of making it as efficient as possible. However, without
  -- a monad transformer, the 'EditText' monad cannot be lifted into 'ReadLines', so the 'ExceptT'
  -- and 'StateT' monad transformers contained within the 'EditText' monad that needs to be
  -- hand-lifted, that is, explicitly evaluated with it's 'StateT state' and ExceptT result threaded
  -- through the 'hReadLines' @fold@ state at every single iteration. This code should be easy for
  -- the compiler to optimize, but it is very inelegant and stateful.
  use editTextTokenizerTable >>= \ table ->
  liftIO (newGapBufferState Nothing $ maybe 256 id size) >>= \ linebuf ->
  ((,) (Right ())) <$> get >>=
  liftIO .
  streamReadLines table linebuf handle
  (\ halt txt -> do
    (_, ed) <- get
    (fileResult, ed) <- liftIO $ runEditText (pushLine Before txt) ed
    put (fileResult, ed)
    case fileResult of
      Left{}   -> halt (fileResult, ed)
      Right () -> pure ()
  ) >>= \ (lineResult, (fileResult, ed)) ->
  put ed >>
  case lineResult of
    Left err -> throwError err
    Right () ->
      case fileResult of
        Left err -> throwError err
        Right () -> pure ()

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
  deriving (Functor, Applicative, Monad, MonadIO, MonadState fold, MonadCont)

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
getLineIndex = editTextLiftGapBuffer . getGaplessIndex . fromIndex

-- | Replace the 'TextLine' at the given 'LineIndex' with the given 'TextLine'.
putLineIndex :: LineIndex -> TextLine tags -> EditText tags ()
putLineIndex i line =
  editTextLiftGapBuffer $ do
    vecIndex <- fromGaplessIndex $ fromIndex i
    liftEditor $ putElemAt vecIndex line

-- | Push a 'TextLine' to 'Before' or 'After' the cursor. This does not effect the current line
-- editor state. If the 'TextLine' is not terminated with a line breaking symbol, the
-- 'editTextLineBreak' symbol is appended before being pushed.
pushLine :: RelativeDirection -> TextLine tags -> EditText tags ()
pushLine dir line = do
  break <- use editTextLineBreak
  editTextLiftGapBuffer $
    pushItem dir $
    ( textLineBreakSymbol %~ \ case
        NoLineBreak -> break
        break       -> break
    ) $
    line

-- | Pop a 'TextLine' from 'Before' or 'After' the cursor. This does not effect the current line
-- edtior state.
popLine :: RelativeDirection -> EditText tags (TextLine tags)
popLine = editTextLiftGapBuffer . popItem

cursorToEnd :: RelativeDirection -> EditText tags RelativeIndex
cursorToEnd dir =
  editTextLiftGapBuffer $
  use (relativeCursor dir) >>= \ count ->
  shiftCursor $ case dir of
    Before -> negate count
    After  -> count

-- | Overwrite the current line in the 'TextEditState' with the content of the current
-- 'LineEditState', do not move the cursor.
flushLine :: Eq (TextLine tags) => EditText tags (TextLine tags)
flushLine = do
  line <- runEditLine editLineFlush
  editTextLiftGapBuffer $ do
    cur <- use beforeCursor
    if cur == 0 then pushItem Before line else
      atCursor Before $ flip putElemAt line
  editTextLineEditor . editLineIsModified .= EditLineBufferLoaded
  pure line

-- | Call 'flushLine' and place the result 'Before' or 'After' the current cursor, then clear the
-- line editor of all characters but a line break at the end of the line. This might not do what you
-- expect since calling this function with 'After' places a newline __before__ the cursor by pushing
-- the current line 'After' the cursor. The 'RelativeDirection' is telling this function where to
-- put the content of the current line, not where to create the new line.
newline :: Eq (TextLine tags) => RelativeDirection -> EditText tags ()
newline dir = do
  lbrk <- use editTextLineBreak
  line <- runEditLine $ do
    line <- editLineFlush
    editLineTextData .= undefinedTextLine
    editLineCursor .= 0
    editLineLiftGB $ do
      beforeCursor .= 0
      afterCursor .= 0
    pure $ textLineBreakSymbol .~ lbrk $ line
  editTextLiftGapBuffer $
    pushItem dir line

-- | This function simply inserts a character unless it is the @'\n'@ character, in which case it
-- calls 'newline' function, all other control characters are inserted literally. This is not a
-- function that respects the line breaking discipline, it is function to be used directly by
-- end-users who probably expect @'\n'@ to insert a line break.
insertChar :: Eq (TextLine tags) => RelativeDirection -> Char -> EditText tags ()
insertChar dir = \ case
  '\n' -> newline dir
  c    -> runEditLine $ editLineLiftGB $ pushItem dir c

-- | Calls 'insertChar' for each item in the given 'String'. This function is simply defined as
-- @'mapM_' ('insertChar' 'Before')@, it is only here becuase it is expected to be used often enough
-- that having a separate function for this feature might be convenient.
insertString :: Eq (TextLine tags) => String -> EditText tags ()
insertString = mapM_ $ insertChar Before

----------------------------------------------------------------------------------------------------

-- | This function starts at the current cursor and moves in the direction 'Before' or 'After' the
-- cursor iterating over each line, passing each line to the folding function along with it's line
-- number, until the cursor has come to the beginning or end of the 'EditTextState' buffer has been
-- reached. The buffer itself is left unmodified, and the cursor will remain in the position it
-- started before this function was called.
foldLinesFromCursor
  :: RelativeDirection
  -> FoldLinesStep tags fold void
  -> fold
  -> EditText tags fold
foldLinesFromCursor dir f fold = case dir of
  Before ->
    editTextLiftGapBuffer gapBuffer3Slice >>= \ (lo, _gap, _hi) ->
    liftIO $
    runFoldTextLines
    ( callCC $ \ halt ->
      let loop i0 =
            let i = i0 - 1 in seq i $!
            liftIO (MVec.read lo i) >>= \ line ->
            f halt (toIndex $ GaplessIndex i) line >>=
            foldTextLineMaybeUpdate (MVec.write lo i) line >>
            if i <= 0 then get else loop i
      in
      loop (MVec.length lo)
    )
    fold
  After ->
    ( editTextLiftGapBuffer $ do
       (_lo, _gap, hi) <- gapBuffer3Slice
       offset <- use beforeCursor
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
-- given 'FoldTextLines' function.
foldLinesInRange
  :: TextRange LineIndex
  -> FoldLinesStep tags fold void
  -> fold
  -> EditText tags fold
foldLinesInRange range f fold = do
  let forward = textRangeIsForward range
  (lo, hi) <- editTextLiftGapBuffer $ do
    let start = fromIndex (range ^. textRangeStart)
    let end   = fromIndex (range ^. textRangeEnd)
    loIndex <- fromGaplessIndex $ min start end
    hiIndex <- fromGaplessIndex $ max start end
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
-- modification.
mapRangeFreeze
  :: Eq tags
  => TextRange LineIndex
  -> (LineIndex -> TextLine tags -> IO a)
  -> EditText tags (Vector a)
mapRangeFreeze range f = do
  let forward = textRangeIsForward range
  editTextLiftGapBuffer $ do
    let start = fromIndex (range ^. textRangeStart)
    let end   = fromIndex (range ^. textRangeEnd)
    loIndex <- fromGaplessIndex $ min start end
    hiIndex <- fromGaplessIndex $ max start end
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
  editTextLiftGapBuffer $
  use (gapBufferEditorState . currentBuffer) >>=
  printOverIOBuffer
  (pure . not . textLineIsUndefined)
  (\ pfx txt ->
     liftIO $
     Strict.putStr pfx >>
     print txt
  )
  (liftIO $ putStrLn "...")
