-- | This module defines a monadic parser 'StringParser' that operates on the 'TextLine' data
-- type. This parser is designed to be used with a 'VecEdit.Text.Line.LineEdit' or
-- 'VecEdit.Text.Editor.TextEdit' function context.
module VecEdit.Text.Parser
  ( -- * String Parser
    StringParser(..), StringParserResult(..), currentTextPoint,
    runStringParser, feedStringParser, parserResultWaiting,
    StringParserState(..), stringParserState,
    parserCharCount, parserStream, parserStreamSeq, parserIsEOF,
    parserCursor, parserRow, parserColumn,
    parserStartPoint, parserLabel,

    -- * Text Buffer Parser
    Parser(..), modifyTags, liftStringParser,
    ParserResult(..), runParserOnRange, runParser,

    -- * Tracking points
    PointParser(..),

    -- * Re-exported modules
    module Text.Parser.Char,
    module Text.Parser.Combinators,
    module Text.Parser.Token
  ) where

import VecEdit.Types
       ( TextPoint(..), Boundary(..), TextBounds, ToIndex(..), IndexValue(..),
         LineIndex(..), CharIndex, textPointRow, textPointColumn,
       )
import VecEdit.Print.DisplayInfo (DisplayInfo(displayInfo), displayInfoShow)
import VecEdit.Text.Internal
       ( TextLine, textLineTags,
         StringLength(stringLength), CharStreamable(toCharStream),
         CharStream, getCharStream, charStreamEnd,
         CharStreamSeq, putCharStreamSeq, takeCharStreamSeq, charStreamSeqNull,
       )
import VecEdit.Text.Editor
       ( EditText, TextBuffer, getLineIndex, maxLineIndex,
       )

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Lens ((.=), (+=), (%=), (^.), (%~), (.~), (+~), Lens', lens, use, assign)
import Control.Monad (MonadPlus(mplus, mzero), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.Reader (MonadReader, ReaderT(..))
import Control.Monad.State.Lazy (StateT(..))
import Control.Monad.State.Class (MonadState(state, get, put))
import Control.Monad.Trans.Class (MonadTrans(lift))

import qualified Data.Text as Strict

import Text.Parser.Char (CharParsing(satisfy, anyChar))
import Text.Parser.Combinators (Parsing((<?>), try, unexpected, notFollowedBy, eof))
import Text.Parser.LookAhead (LookAheadParsing(lookAhead))
import Text.Parser.Token (TokenParsing())

----------------------------------------------------------------------------------------------------

-- | An 'StringParser'  is a kind of  string parser that operates on  'CharStreamable' stringss. The
-- 'StringParserState' has  an index value  of type 'Int'  that points to  a position in  the string
-- @str@. When the end  of a string is reached the parsers enters  into it's 'ParserWait' state. You
-- can insert another @str@ of input, increment 'theParserLineIndex', and resume parsing.
--
-- The 'StringParser' instantiates the 'CharParsing' type class of the "parsers" package.
newtype StringParser m a
  = StringParser
    { stringParserStateT ::
        StateT StringParserState m (StringParserResult m a)
    }
  deriving Functor

-- | The state  of an 'StringParser' keeps track of  the current string @str@, and the  index in the
-- current string that the parser is inspecting. There  is also a counter keeping track of the total
-- number of characters  inspected, and a counter keeping  track of the total number  of lines being
-- inspected.
--
-- Initialize   a   new   'StringParserState'    with   'stringParserState',   and   then   evaluate
-- 'feedStringParser' on  the state value for  every line of input  you receive. Lines needs  not be
-- broken on line breaking  characters, but 'parserIsEOF' should be set to 'True'  on the final line
-- of input that is to be parsed.
data StringParserState
  = StringParserState
    { theParserStream :: !CharStream
      -- ^ Keeps track of the current string being parsed.
    , theParserStreamSeq :: !CharStreamSeq
      -- ^ If more 'CharStream's are appended, they  are stored here in this FIFO. Parsing continues
      -- until this FIFO is emptied, then the parser enters  a wait state where you can feed it more
      -- input.
    , theParserCharCount :: !Int
      -- ^ Keeps track of the number of characters that have been parsed.
    , theParserIsEOF :: !Bool
      -- ^ Indicates whether this parser  is at the end of it's input. If this  is set to 'True' and
      -- 'getCharStream' is returning 'Nothing' on 'theParserStream', then this parser is at the end
      -- of the file. , and also 'theParserStreamSeq' is empty according to 'charStreamSeqNull'.
    , theParserCursor :: !TextPoint
      -- ^ Keeps track of the total number of characters that have been parsed across all lines.
    , theParserStartPoint :: !TextPoint
      -- ^  Keeps track  of 'theParserIndex'  and 'theParserLineIndex'  of where  the current  label
      -- ('theParserLabel') was set so  that the range of characters for that  label can be reported
      -- if an parse error should occur.
    , theParserLabel :: !Strict.Text
      -- ^ Keeps track of the name of the parser for a time, used in error reporting.
    }

-- | Construct a new 'StringParserState' from a  @str@ value. This starts 'parserStartPoint' on line
-- 0 so  that you can initializae  a fresh copy of  this state value with  'feedStringParser' on the
-- first line of  the input that you  need to parse. The 'feedStringParser'  function increments the
-- line number,  so running 'feedStringParser'  on a  fresh 'stringParserState' increments  the line
-- number frome line 0 to line 1.
stringParserState :: StringParserState
stringParserState =
  StringParserState
  { theParserStream = charStreamEnd
  , theParserStreamSeq = mempty
  , theParserCharCount = 0
  , theParserIsEOF = False
  , theParserCursor = line0
  , theParserStartPoint = line0
  , theParserLabel = ""
  }
  where
  line0 =
    TextPoint
    { theTextPointRow = 0
    , theTextPointColumn = 0
    }

-- | Keeps track of the number of characters that have been parsed.
parserCharCount :: Lens' StringParserState Int
parserCharCount = lens theParserCharCount $ \ a b -> a{ theParserCharCount = b }

-- | Not for export
getParserState :: Monad m => StringParser m StringParserState
getParserState = StringParser $ ParseOK <$> get

-- | Not for export
putParserState :: Monad m => StringParserState -> StringParser m ()
putParserState = StringParser . fmap ParseOK . put

-- | Keeps track of the current string being parsed.
parserStream :: Lens' StringParserState CharStream
parserStream = lens theParserStream $ \ a b -> a{ theParserStream = b }

-- | If more 'CharStream's are appended, they are  stored here in this FIFO. Parsing continues until
-- this FIFO is emptied, then the parser enters a wait state where you can feed it more input.
parserStreamSeq :: Lens' StringParserState CharStreamSeq
parserStreamSeq = lens theParserStreamSeq $ \ a b -> a{ theParserStreamSeq = b }

-- | Keeps  track of  whether the current  line of  input is  the last line  of input.   However the
-- 'ParserState'   is  not   truly   at   the  end-of-file   until   'getCharStream'  evaluated   on
-- 'theParserStream' is  producing 'Nothing',  and also 'theParserStreamSeq'  is empty  according to
-- 'charStreamSeqNull'.
parserIsEOF :: Lens' StringParserState Bool
parserIsEOF = lens theParserIsEOF $ \ a b -> a{ theParserIsEOF = b }

-- | Keeps track of the 'TextPoint' of the 'StringParser'.
parserCursor :: Lens' StringParserState TextPoint
parserCursor = lens theParserCursor $ \ a b -> a{ theParserCursor = b }

-- | A convenient lens into the 'parserCursor' with 'textPointRow'.
parserRow :: Lens' StringParserState LineIndex
parserRow = parserCursor . textPointRow

-- | A convenient lens into the 'parserCursor' with 'textPointColumn
parserColumn :: Lens' StringParserState CharIndex
parserColumn = parserCursor . textPointColumn

-- | Keeps track of 'theParserIndex' and 'theParserLineIndex' of where the current label
-- ('theParserLabel') was set so that the range of characters for that label can be reported if an
-- parse error should occur. Using the '<?>' combinator sets this value to be equal to the value
-- returned by 'currentTextPoint'. You can also use 'parserResetStartPoint'.
parserStartPoint :: Lens' StringParserState TextPoint
parserStartPoint = lens theParserStartPoint $ \ a b -> a{ theParserStartPoint = b }

-- | Keeps track of the name of the parser for a time, used in error reporting.
parserLabel :: Lens' StringParserState Strict.Text
parserLabel = lens theParserLabel $ \ a b -> a{ theParserLabel = b }

-- | The 'StringParserResult' is a value that decides how to proceed with the next step of the
-- parser (i.e. the left hand of the monadic bind operator).
data StringParserResult m a
  = NoMatch
  | ParseError !Strict.Text
  | ParseWait (StringParser m a)
  | ParseOK a
  deriving Functor

instance Show a => Show (StringParserResult m a) where
  show = \ case
    NoMatch        -> "(empty)"
    ParseError err -> "(fail " <> show err <> ")"
    ParseWait{}    -> "(await-input)"
    ParseOK    a   -> "(ok " <> show a <> ")"

instance Monad m => Monad (StringParser m) where
  return = StringParser . return . ParseOK
  (StringParser f) >>= ma = StringParser $ f >>= \ case
    NoMatch         -> pure NoMatch
    ParseWait  p    -> pure $ ParseWait $ p >>= ma
    ParseError err  -> pure $ ParseError err
    ParseOK    a    -> stringParserStateT $ ma a

instance Monad m => MonadPlus (StringParser m) where
  mzero = StringParser $ pure NoMatch
  mplus (StringParser left) (StringParser right) =
    StringParser $
    left >>= \ case
      NoMatch        -> right
      ParseWait left -> pure $ ParseWait $ mplus left $ StringParser right
      left           -> pure left

instance Monad m => Applicative (StringParser m) where
  pure = StringParser . pure . ParseOK
  (<*>) = ap

instance Monad m => Alternative (StringParser m) where
  empty = mzero
  (<|>) = mplus

instance MonadIO m => MonadIO (StringParser m) where
  liftIO = StringParser . fmap ParseOK . liftIO

instance MonadTrans (StringParser) where
  lift = StringParser . fmap ParseOK . lift

instance Monad m => MonadError Strict.Text (StringParser m) where
  throwError err = StringParser $ pure $ ParseError err
  catchError (StringParser f) catch =
    StringParser $
    f >>= \ case
      ParseError err -> stringParserStateT (catch err)
      a -> pure a

instance Monad m => MonadFail (StringParser m) where
  fail = throwError . Strict.pack

instance Show a => DisplayInfo (StringParserResult m a) where
  displayInfo putStr = \ case
    NoMatch        -> putStr "(empty)"
    ParseError err -> putStr "(fail " >> displayInfoShow putStr err >> putStr ")"
    ParseWait{}    -> putStr "(await-input)"
    ParseOK    a   -> putStr "(ok " >> displayInfoShow putStr a >> putStr ")"

instance Show StringParserState where
  show st =
    "(parser " <> show (show $ theParserStream st) <>
    " :line " <> show (st ^. parserRow) <>
    " :column " <> show (st ^. parserColumn) <>
    " :char-count " <> show (st ^. parserCharCount) <>
    " :start-point (" <>
    let pt = theParserStartPoint st in
    show (theTextPointRow pt) <> " " <>
    show (theTextPointColumn pt) <>
    ") :label " <> show (theParserLabel st) <>
    ")"

instance DisplayInfo StringParserState where
  displayInfo putStr st = do
    putStr "(parser\n "
    displayInfoShow putStr $ show $ theParserStream st
    putStr "\n  :line "
    displayInfoShow putStr (st ^. parserRow)
    putStr "\n  :is-eof "
    displayInfoShow putStr (st ^. parserIsEOF)
    putStr "\n  :more "
    displayInfoShow putStr (not $ charStreamSeqNull $ st ^. parserStreamSeq)
    putStr "\n  :column "
    displayInfoShow putStr (st ^. parserColumn)
    putStr "\n  :char-count "
    displayInfoShow putStr (st ^. parserCharCount)
    let start = theParserStartPoint st
    putStr "\n  :start-point ("
    displayInfoShow putStr $ theTextPointRow start
    putStr " "
    displayInfoShow putStr $ theTextPointColumn start
    putStr "\n)\n  :label "
    displayInfoShow putStr $ theParserLabel st
    putStr ")"

-- | Run an 'StringParser' function in the monad @m@ on the given 'ParserState'.
runStringParser
  :: Monad m
  => StringParser m a
  -> StringParserState
  -> m (StringParserResult m a, StringParserState)
runStringParser (StringParser f) = runStateT f

-- | Update a 'StringParserState' with a new line of text, and a 'Bool' indicating whether this is
-- the last line of text. It is assumed the previous line ended at a line break, though this is not
-- checked.
feedStringParser :: CharStreamable str => StringParserState -> Bool -> str -> StringParserState
feedStringParser st eof string =
  (parserRow +~ 1) .
  (parserColumn .~ 1) .
  (parserIsEOF .~ eof) .
  ( parserStreamSeq %~
    putCharStreamSeq (toCharStream string charStreamEnd)
  ) $
  st

-- | Evaluates to 'True' if the given 'StringParserResult' is waiting for more input. When
-- recursively calling on 'resumeStringParser', use this function to end the recursion.
parserResultWaiting :: StringParserResult m a -> Bool
parserResultWaiting = \ case
  ParseWait{} -> True
  _ -> False

-- | Get the current 'TextPoint'.
currentTextPoint :: Monad m => StringParser m TextPoint
currentTextPoint = StringParser $ ParseOK . (^. parserCursor) <$> get

-- | This class is instantiated by both of the parsing monads defined in this module.
class Monad m => PointParser m where
  -- | Set 'parserStartPoint' to the 'currentTextPoint' value, useful when you want to define the
  -- start of a token without using the '<?>' combinator.
  parserResetStartPoint :: m ()

instance Monad m => PointParser (StringParser m) where
  parserResetStartPoint =
    currentTextPoint >>=
    StringParser .
    fmap ParseOK .
    assign parserStartPoint

----------------------------------------------------------------------------------------------------

-- Instantiation of the type classes from the @parsers@ package.

instance Monad m => Parsing (StringParser m) where

  try p =
    getParserState >>= \ st ->
    p <|> putParserState st *> empty

  notFollowedBy (StringParser p) =
    getParserState >>=
    lift . runStateT p >>= \ (result, _st) ->
    case result of
      ParseOK{}   -> mzero
      ParseWait p -> StringParser $ pure $ ParseWait (notFollowedBy p)
      _           -> pure ()

  (<?>) p label0 = do
    let label = Strict.pack label0
    point <- currentTextPoint
    oldLabel <- StringParser $
      ParseOK <$>
      use parserLabel <*
      (parserLabel .= label) <*
      (parserStartPoint .= point)
    p <* StringParser (ParseOK <$> (parserLabel .= oldLabel))

  unexpected msg = throwError $ Strict.pack $ "unexpected " <> msg

  eof = StringParser $ do
    st <- get
    let isEOF  = st ^. parserIsEOF
    let stream = st ^. parserStream
    let count  = st ^. parserCharCount
    let sequ   = st ^. parserStreamSeq
    case getCharStream count stream of
      Left count | isEOF && charStreamSeqNull sequ ->
        ParseOK <$> (parserCharCount .= count)
      _ ->
        pure NoMatch

instance Monad m => CharParsing (StringParser m) where

    satisfy ok = StringParser $ do
      st <- get
      let isEOF  = st ^. parserIsEOF
      let stream = st ^. parserStream
      let count  = st ^. parserCharCount
      case getCharStream count stream of
        Left count ->
          let sequ = st ^. parserStreamSeq in
          case takeCharStreamSeq sequ of
            (Just cs, sequ) -> do
              parserStream .= cs
              parserStreamSeq .= sequ
              stringParserStateT $ satisfy ok
            (Nothing, _) | not isEOF -> do
              parserStream .= stream
              parserCharCount .= count
              pure (ParseWait $ satisfy ok)
            _ -> pure NoMatch
        Right (count, c, next) ->
          if ok c then do
            parserStream .= next
            parserCharCount .= count
            parserColumn += 1
            pure $ ParseOK c
          else
            pure $ NoMatch

instance Monad m => LookAheadParsing (StringParser m) where

  lookAhead = (getParserState >>=) . (. putParserState) . (<*)

instance Monad m => TokenParsing (StringParser m)

----------------------------------------------------------------------------------------------------

-- | The most common use of this function type is to define parsers for syntax coloring. This
-- 'Parser' function type is an ordinary 'StringParser' with some additional information so that it
-- can be applied to a 'TextBufer'. This function type instantiates the 'Parsing' typeclass, so you
-- should define parsers using the 'Parsing' typeclass APIs, and then evaluate these parsers on a
-- 'TextBuffer' to extract information about the text in the 'TextBuffer' using the 'runParser' or
-- 'runParserOnRange' functions.
newtype Parser fold tags a
  = Parser
    { unwrapParser ::
        ReaderT
        (TextBuffer tags)
        (StringParser (StateT (ParserState fold tags) IO))
        a
    }
  deriving
  ( Functor, Applicative, Alternative
  , Monad, MonadPlus, MonadIO
  , MonadReader (TextBuffer tags)
  )

instance MonadFail (Parser fold tags) where
  fail = throwError . Strict.pack

instance MonadState fold (Parser fold tags) where
  state f = Parser $ lift $ lift $ state $ \ st ->
    let (a, fold) = f $ st ^. parserFoldFold in
    (a, parserFoldFold .~ fold $ st)

instance MonadError Strict.Text (Parser fold tags) where
  throwError = Parser . lift . throwError
  catchError (Parser try) catch =
    Parser $ ReaderT $ \ env ->
    catchError (runReaderT try env) $ \ err ->
    runReaderT (unwrapParser $ catch err) env

data ParserState fold tags
  = ParserState
    { theParserStateFold :: !fold
    , theParserStateLine :: !(TextLine tags)
    }

parserFoldFold :: Lens' (ParserState fold tags) fold
parserFoldFold = lens theParserStateFold $ \ a b -> a{ theParserStateFold = b }

parserFoldLine :: Lens' (ParserState fold tags) (TextLine tags)
parserFoldLine = lens theParserStateLine $ \ a b -> a{ theParserStateLine = b }

-- | Information about a 'Parser' failure occuring as a result of 'runParser' or
-- 'runParserOnRange'. This information is extracted from the 'StringParserResult'.
data ParserResult fold tags a
  = ParserResult
    { theParserState :: !StringParserState
    , theParserLine :: !(TextLine tags)
    , theParserFold :: !fold
    , theParserResult :: !(Either Strict.Text a)
    }

-- | Evaluate a 'StringParser' function in the current 'Parser' function context.
liftStringParser
  :: StringParser (StateT (ParserState fold tags) IO) a
  -> Parser fold tags a
liftStringParser = Parser . lift

-- | Perform some update on the @tags@ value of the current 'TextLine' that is being inspected by
-- the 'Parser'. This function obviously is specific to this 'Parser' and is not portable, but it is
-- the means by which you can assign tags denoting parts of syntax to a line of text ('TextLine') in
-- the current text buffer.
modifyTags :: (Maybe tags -> Maybe tags) -> Parser fold tags ()
modifyTags f = Parser $ lift $ lift $ parserFoldLine . textLineTags %= f

-- | Like 'runParserOnRange', but evaluates the 'Parser' on the whole 'TextBuffer'.
runParser
  :: Eq (TextLine tags)
  => Bool
  -> fold
  -> Parser fold tags a
  -> EditText tags [ParserResult fold tags a]
runParser doRepeat fold p0 =
  maxLineIndex >>= \ maxLine ->
  getLineIndex maxLine >>= \ finalLine ->
  flip (runParserOnRange doRepeat fold) p0 $
  Boundary
  { theBoundaryStart =
      TextPoint
      { theTextPointRow = 1
      , theTextPointColumn = 1
      }
  , theBoundaryEnd =
      TextPoint
      { theTextPointRow = maxLine
      , theTextPointColumn = toIndex $ wrapIndex $ stringLength finalLine
      }
  }

-- | Evaluate a 'Parser' function on an entire 'Range' of text in a 'TextBuffer'. If the 'Parser'
-- runs to completion before the entire 'Range' is parsed, parsing may resume on the very next
-- character after the previous 'Parse' completed if the first 'Bool' argument given to this
-- function is 'True'.
runParserOnRange
  :: forall fold tags a
  . Eq (TextLine tags)
  => Bool
  -> fold
  -> TextBounds
  -> Parser fold tags a
  -> EditText tags [ParserResult fold tags a]
runParserOnRange doRepeat fold range p0 =
  if  firstRow > lastRow ||
      firstRow == lastRow && firstColumn < lastColumn
  then pure []
  else getLineIndex firstRow >>= init
  where
  TextPoint{theTextPointRow=firstRow, theTextPointColumn=firstColumn} = theBoundaryStart range
  TextPoint{theTextPointRow=lastRow , theTextPointColumn=lastColumn } = theBoundaryEnd   range
  start =
    TextPoint
    { theTextPointRow = firstRow
    , theTextPointColumn = firstColumn
    }
  init :: TextLine tags -> EditText tags [ParserResult fold tags a]
  init line =
    loop id p0
    (ParserState{ theParserStateFold = fold, theParserStateLine = line }) $
    stringParserState
    { theParserStream = toCharStream line charStreamEnd
    , theParserCursor = start
    , theParserStartPoint = start
    }
  loop
    :: ([ParserResult fold tags a] -> [ParserResult fold tags a])
    -> Parser fold tags a
    -> ParserState fold tags
    -> StringParserState
    -> EditText tags [ParserResult fold tags a]
  loop stack (Parser p) fold parst = do
    editor <- get
    ((result, parst), fold) <- liftIO $
      runStateT (runStringParser (runReaderT p editor) parst) fold
    let elem =
          ParserResult
          { theParserState = parst
          , theParserLine = theParserStateLine fold
          , theParserFold = theParserStateFold fold
          , theParserResult = Left ""
          }
    case result of
      NoMatch         -> pure $ stack . (elem :) $ []
      ParseError err  -> pure $ stack . ((elem{ theParserResult = Left err }) :) $ []
      ParseOK    a    ->
        if doRepeat then
          loop (stack . ((elem{ theParserResult = Right a }) :)) p0 fold parst
        else
          pure $ stack . ((elem{ theParserResult = Right a }) :) $ []
      ParseWait  p    ->
        if (parst ^. parserRow) >= lastRow then
          if theParserIsEOF parst then
            pure $ stack . ((elem{ theParserResult = Left "unexpected EOF" }) :) $ []
          else
            loop stack (Parser $ lift p) fold (parst{ theParserIsEOF = True })
        else
          let nextRow = (parst ^. parserRow) + 1 in
          getLineIndex nextRow >>= \ line ->
          loop stack (Parser $ lift p) fold $
          parst
          { theParserStreamSeq =
              putCharStreamSeq
              (toCharStream line charStreamEnd)
              (theParserStreamSeq parst)
          , theParserIsEOF = nextRow >= lastRow
          , theParserCursor =
              TextPoint
              { theTextPointRow = nextRow
              , theTextPointColumn = 1
              }
          }

----------------------------------------------------------------------------------------------------

instance Parsing (Parser fold tags) where

  try (Parser f) = Parser $ ReaderT $ try . runReaderT f

  notFollowedBy (Parser f) = Parser $ ReaderT $ notFollowedBy . runReaderT f

  (<?>) (Parser f) newName =
    Parser $ ReaderT $ \ env ->
    runReaderT f env <?> newName

  unexpected = liftStringParser . unexpected

  eof = liftStringParser eof

----------------------------------------------------------------------------------------------------

instance LookAheadParsing (Parser fold tags) where
  lookAhead (Parser f) = Parser $ ReaderT $ lookAhead . runReaderT f

----------------------------------------------------------------------------------------------------

instance CharParsing (Parser fold tags) where
  satisfy = liftStringParser . satisfy

instance PointParser (Parser fold tags) where
  parserResetStartPoint = liftStringParser parserResetStartPoint

instance TokenParsing (Parser fold tags)
