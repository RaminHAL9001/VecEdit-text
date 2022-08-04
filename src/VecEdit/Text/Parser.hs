-- | 
module VecEdit.Text.Parser
  ( -- * String Parser
    StringParser(..), StringParserResult(..), currentTextPoint,
    runStringParser, resumeStringParser, parserResultWaiting,
    StringParserState(..), stringParserState,
    parserString, parserIndex, parserCharCount, parserLineIndex,
    parserLabel, parserStartPoint, parserIsEOF, parserTextPoint,

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
       ( TextPoint(..), TextRange(..), VectorIndex, VectorSize,
         ToIndex(..), FromIndex(..), IndexValue(..), GaplessIndex(..),
         LineIndex(..), CharIndex
       )
import VecEdit.Print.DisplayInfo (DisplayInfo(..), displayInfoShow)
import VecEdit.Text.String
       ( TextLine, StringLength(..), IOIndexableString(..), CuttableString(..), textLineTags)
import VecEdit.Text.Editor
       ( EditText, TextBuffer, getLineIndex, maxLineIndex,
       )

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Lens ((.=), (+=), (%=), Lens', lens, use, assign)
import Control.Monad (MonadPlus(mplus, mzero), guard, ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.Reader (MonadReader, ReaderT(..))
import Control.Monad.State.Lazy (StateT(..), State, runState)
import Control.Monad.State.Class (MonadState(state, get, put))
import Control.Monad.Trans.Class (MonadTrans(lift))

import qualified Data.Text as Strict

import Text.Parser.Char (CharParsing(satisfy, anyChar))
import Text.Parser.Combinators (Parsing((<?>), try, unexpected, notFollowedBy, eof))
import Text.Parser.LookAhead (LookAheadParsing(lookAhead))
import Text.Parser.Token (TokenParsing())

----------------------------------------------------------------------------------------------------

-- | An 'StringParser' is a kind of string parser that operates on 'IndexableString's. The
-- 'StringParserState' has an index value of type 'Int' that points to a position in the string
-- @str@. When the end of a string is reached the parsers enters into it's 'ParserWait' state. You
-- can insert another @str@ of input, increment 'theParserLineIndex', and resume parsing.
--
-- The 'StringParser' instantiates the 'CharParsing' type class of the "parsers" package.
newtype StringParser str m a
  = StringParser
    { stringParserStateT ::
        StateT (StringParserState str) m (StringParserResult str m a)
    }
  deriving Functor

-- | The state of an 'StringParser' keeps track of the current string @str@, and the index in the
-- current string that the parser is inspecting. There is also a counter keeping track of the total
-- number of characters inspected, and a counter keeping track of the total number of lines being
-- inspected.
data StringParserState str
  = StringParserState
    { theParserString :: !str
      -- ^ Keeps track of the current string being parsed.
    , theParserIndex :: !VectorIndex
      -- ^ Keeps track of the current index in the current 'parserString'.
    , theParserCharCount :: !VectorSize
      -- ^ Keeps track of the total number of characters that have been parsed across all lines.
    , theParserLineIndex :: !LineIndex
      -- ^ Keeps track of the total number of lines that have been parsed.
    , theParserLabel :: !Strict.Text
      -- ^ Keeps track of the name of the parser for a time, used in error reporting.
    , theParserStartPoint :: !TextPoint
      -- ^ Keeps track of 'theParserIndex' and 'theParserLineIndex' of where the current label
      -- ('theParserLabel') was set so that the range of characters for that label can be reported
      -- if an parse error should occur.
    , theParserIsEOF :: !Bool
      -- ^ The parser can be set into it's end-of-file state when there is no more input, and if
      -- 'theParserIndex' has gone past the end of the current @str@ and this flag is set to 'True',
      -- then the 'eof' parser will return 'True'.
    }

-- | Construct a new 'StringParserState' from a @str@ value.
stringParserState :: str -> StringParserState str
stringParserState str =
  StringParserState
  { theParserString = str
  , theParserIndex = 0
  , theParserCharCount = 0
  , theParserLineIndex = LineIndex 1
  , theParserLabel = ""
  , theParserStartPoint = TextPoint 1 1
  , theParserIsEOF = False
  }

-- | Not for export
getParserState :: Monad m => StringParser str m (StringParserState str)
getParserState = StringParser $ ParseOK <$> get

-- | Not for export
putParserState :: Monad m => StringParserState str -> StringParser str m ()
putParserState = StringParser . fmap ParseOK . put

-- | Not for export
onStringParserState :: Monad m => State (StringParserState str) a -> StringParser str m a
onStringParserState = StringParser . fmap ParseOK . state . runState

-- | Keeps track of the current string being parsed.
parserString :: Lens' (StringParserState str) str
parserString = lens theParserString $ \ a b -> a{ theParserString = b }

-- | Keeps track of the current index in the current 'parserString'.
parserIndex :: Lens' (StringParserState str) Int
parserIndex = lens theParserIndex $ \ a b -> a{ theParserIndex = b }

-- | Keeps track of the total number of characters that have been parsed across all lines.
parserCharCount :: Lens' (StringParserState str) Int
parserCharCount = lens theParserCharCount $ \ a b -> a{ theParserCharCount = b }

-- | Keeps track of the line number.
parserLineIndex :: Lens' (StringParserState str) LineIndex
parserLineIndex = lens theParserLineIndex $ \ a b -> a{ theParserLineIndex = b }

-- | Keeps track of the name of the parser for a time, used in error reporting.
parserLabel :: Lens' (StringParserState str) Strict.Text
parserLabel = lens theParserLabel $ \ a b -> a{ theParserLabel = b }

-- | Keeps track of 'theParserIndex' and 'theParserLineIndex' of where the current label
-- ('theParserLabel') was set so that the range of characters for that label can be reported if an
-- parse error should occur. Using the '<?>' combinator sets this value to be equal to the value
-- returned by 'currentTextPoint'. You can also use 'parserResetStartPoint'.
parserStartPoint :: Lens' (StringParserState str) TextPoint
parserStartPoint = lens theParserStartPoint $ \ a b -> a{ theParserStartPoint = b }

-- | The parser can be set into it's end-of-file state when there is no more input, and if
-- 'theParserIndex' has gone past the end of the current @str@ and this flag is set to 'True', then
-- the 'eof' parser will return 'True'.
parserIsEOF :: Lens' (StringParserState str) Bool
parserIsEOF = lens theParserIsEOF $ \ a b -> a{ theParserIsEOF = b }

-- | The 'StringParserResult' is a value that decides how to proceed with the next step of the
-- parser (i.e. the left hand of the monadic bind operator).
data StringParserResult str m a
  = NoMatch
  | ParseError !Strict.Text
  | ParseWait (StringParser str m a)
  | ParseOK a
  deriving Functor

instance Show a => Show (StringParserResult str m a) where
  show = \ case
    NoMatch        -> "(empty)"
    ParseError err -> "(fail " <> show err <> ")"
    ParseWait{}    -> "(await-input)"
    ParseOK    a   -> "(ok " <> show a <> ")"

instance Monad m => Monad (StringParser str m) where
  return = StringParser . return . ParseOK
  (StringParser f) >>= ma = StringParser $ f >>= \ case
    NoMatch         -> pure NoMatch
    ParseWait  p    -> pure $ ParseWait $ p >>= ma
    ParseError err  -> pure $ ParseError err
    ParseOK    a    -> stringParserStateT $ ma a

instance Monad m => MonadPlus (StringParser str m) where
  mzero = StringParser $ pure NoMatch
  mplus (StringParser left) (StringParser right) =
    StringParser $
    left >>= \ case
      NoMatch        -> right
      ParseWait left -> pure $ ParseWait $ mplus left $ StringParser right
      left           -> pure left

instance Monad m => Applicative (StringParser str m) where
  pure = StringParser . pure . ParseOK
  (<*>) = ap

instance Monad m => Alternative (StringParser str m) where
  empty = mzero
  (<|>) = mplus

instance MonadIO m => MonadIO (StringParser str m) where
  liftIO = StringParser . fmap ParseOK . liftIO

instance MonadTrans (StringParser str) where
  lift = StringParser . fmap ParseOK . lift

instance Monad m => MonadError Strict.Text (StringParser str m) where
  throwError err = StringParser $ pure $ ParseError err
  catchError (StringParser f) catch =
    StringParser $
    f >>= \ case
      ParseError err -> stringParserStateT (catch err)
      a -> pure a

instance Monad m => MonadFail (StringParser str m) where
  fail = throwError . Strict.pack

instance Show a => DisplayInfo (StringParserResult str m a) where
  displayInfo putStr = \ case
    NoMatch        -> putStr "(empty)"
    ParseError err -> putStr "(fail " >> displayInfoShow putStr err >> putStr ")"
    ParseWait{}    -> putStr "(await-input)"
    ParseOK    a   -> putStr "(ok " >> displayInfoShow putStr a >> putStr ")"

instance Show str => Show (StringParserState str) where
  show st =
    "(parser :index " <> show (theParserIndex st) <>
    " :line " <> show (theParserLineIndex st) <>
    " :char-count " <> show (theParserCharCount st) <>
    " :is-EOF " <> show (theParserIsEOF st) <>
    " :label " <> show (theParserLabel st) <>
    " :start-point (" <>
    let pt = theParserStartPoint st in
    show (theTextPointRow pt) <> " " <>
    show (theTextPointColumn pt) <>
    ") :input " <> show (theParserString st) <>
    ")"

instance Show str => DisplayInfo (StringParserState str) where
  displayInfo putStr st = do
    putStr "(parser\n  :index "
    displayInfoShow putStr $ theParserIndex st
    putStr "\n  :line "
    displayInfoShow putStr $ theParserLineIndex st
    putStr "\n  :char-count "
    displayInfoShow putStr $ theParserCharCount st
    putStr "\n  :is-EOF "
    displayInfoShow putStr $ theParserIsEOF st
    putStr "\n  :label "
    displayInfoShow putStr $ theParserLabel st
    let start = theParserStartPoint st
    putStr "\n  :start-point ("
    displayInfoShow putStr $ theTextPointRow start
    putStr " "
    displayInfoShow putStr $ theTextPointColumn start
    putStr ")\n  :input\n  "
    displayInfoShow putStr $ theParserString st
    putStr ")\n"

-- | Run an 'StringParser' function in the monad @m@ on the given 'ParserState'.
runStringParser
  :: Monad m
  => StringParser str m a
  -> StringParserState str
  -> m (StringParserResult str m a, StringParserState str)
runStringParser (StringParser f) = runStateT f

-- | Run a 'StringParser' function contained within a 'StringParserResult' value.  If the
-- 'StringParserResult' is not 'ParseWait', this function does nothing and returns both arguments
-- unchanged as a pair (2-tuple). The 'uncurry'-ied form of this function can evaluate on the result
-- of the 'runStringParser' function.
resumeStringParser
  :: Monad m
  => StringParserResult str m a
  -> StringParserState str
  -> m (StringParserResult str m a, StringParserState str)
resumeStringParser = \ case
  NoMatch        -> pure . (,) NoMatch
  ParseWait  p   -> runStringParser p
  ParseError err -> pure . (,) (ParseError err)
  ParseOK    a   -> pure . (,) (ParseOK a)

-- | Evaluates to 'True' if the given 'StringParserResult' is waiting for more input. When
-- recursively calling on 'resumeStringParser', use this function to end the recursion.
parserResultWaiting :: StringParserResult str m a -> Bool
parserResultWaiting = \ case
  ParseWait{} -> True
  _ -> False

-- | Extract the 'TextPoint' value from the given 'StringParserState'.
parserTextPoint :: StringParserState str -> TextPoint
parserTextPoint st =
  TextPoint
  { theTextPointRow = theParserLineIndex st
  , theTextPointColumn = toIndex $ GaplessIndex $ theParserIndex st
  }

-- | Get the current 'TextPoint'.
currentTextPoint :: Monad m => StringParser str m TextPoint
currentTextPoint = StringParser $ ParseOK . parserTextPoint <$> get

-- | This class is instantiated by both of the parsing monads defined in this module.
class Monad m => PointParser m where
  -- | Set 'parserStartPoint' to the 'currentTextPoint' value, useful when you want to define the
  -- start of a token without using the '<?>' combinator.
  parserResetStartPoint :: m ()

instance Monad m => PointParser (StringParser str m) where
  parserResetStartPoint =
    currentTextPoint >>=
    StringParser .
    fmap ParseOK .
    assign parserStartPoint

----------------------------------------------------------------------------------------------------

-- Instantiation of the type classes from the @parsers@ package.

instance (Monad m, StringLength str) => Parsing (StringParser str m) where

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

  eof = do
    (i, str, isEOF) <-
      StringParser $
      (\ st ->
        ParseOK
        ( theParserIndex st
        , theParserString st
        , theParserIsEOF st
        )
      ) <$> get
    guard $ isEOF && i >= stringLength str

instance
  (MonadIO m, StringLength str, IOIndexableString str)
  => CharParsing (StringParser str m) where

    satisfy ok =
      ( StringParser $
        (\ st ->
          ParseOK (theParserIndex st, theParserString st)
        ) <$> get
      ) >>= \ (i, str) ->
      if i >= stringLength str then
        StringParser $ pure $ ParseWait $ satisfy ok
      else
        liftIO (charAtIndexIO str i) >>= \ c ->
        if ok c then
          StringParser
          ( ParseOK <$>
            ( parserIndex += 1 >>
              parserCharCount += 1
            )
          ) *> pure c
        else empty

instance (Monad m, StringLength str) => LookAheadParsing (StringParser str m) where

  lookAhead = (getParserState >>=) . (. putParserState) . (<*)

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
        (StringParser (TextLine tags) (StateT fold IO))
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
  state = Parser . lift . lift . state

instance MonadError Strict.Text (Parser fold tags) where
  throwError = Parser . lift . throwError
  catchError (Parser try) catch =
    Parser $ ReaderT $ \ env ->
    catchError (runReaderT try env) $ \ err ->
    runReaderT (unwrapParser $ catch err) env

-- | Information about a 'Parser' failure occuring as a result of 'runParser' or
-- 'runParserOnRange'. This information is extracted from the 'StringParserResult'.
data ParserResult fold tags a
  = ParserResult
    { theParserState :: !(StringParserState (TextLine tags))
    , theParserFold :: !fold
    , theParserResult :: !(Either Strict.Text a)
    }

-- | Evaluate a 'StringParser' function in the current 'Parser' function context.
liftStringParser :: StringParser (TextLine tags) (StateT fold IO) a -> Parser fold tags a
liftStringParser = Parser . lift

-- | Not for export. Evaluate a 'State' function that can inspect or modify the 'StringParserState'
-- of the current 'Parser' function context.
onParserState :: State (StringParserState (TextLine tags)) a -> Parser fold tags a
onParserState = Parser . lift . onStringParserState

-- | Perform some update on the @tags@ value of the current 'TextLine' that is being inspected by
-- the 'Parser'. This function obviously is specific to this 'Parser' and is not portable, but it is
-- the means by which you can assign tags denoting parts of syntax to a line of text ('TextLine') in
-- the current text buffer.
modifyTags :: (Maybe tags -> Maybe tags) -> Parser fold tags ()
modifyTags f = onParserState $ parserString . textLineTags %= f

-- | Like 'runParserOnRange', but evaluates the 'Parser' on the whole 'TextBuffer'.
runParser
  :: Bool
  -> fold
  -> Parser fold tags a
  -> EditText tags [ParserResult fold tags a]
runParser doRepeat fold p0 =
  maxLineIndex >>= \ maxLine ->
  getLineIndex maxLine >>= \ finalLine ->
  flip (runParserOnRange doRepeat fold) p0 $
  TextRange
  { theTextRangeStart =
      TextPoint
      { theTextPointRow = 1
      , theTextPointColumn = 1
      }
  , theTextRangeEnd =
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
  :: Bool
  -> fold
  -> TextRange TextPoint
  -> Parser fold tags a
  -> EditText tags [ParserResult fold tags a]
runParserOnRange doRepeat fold range p0 =
  if firstRow > lastRow then pure []
  else if firstRow == lastRow then
    if firstColumn < lastColumn then pure [] else
    getLineIndex firstRow >>= \ line ->
    init $
    cutFromEnd (unCharIndex lastColumn) $
    cutFromStart (unCharIndex firstColumn) $
    line
  else
    getLineIndex firstRow >>= init
  where
  init line =
    loop id fold p0 $
    (stringParserState line)
    { theParserLineIndex = firstRow
    , theParserIndex = unwrapIndex (fromIndex firstColumn)
    , theParserStartPoint =
        TextPoint
        { theTextPointRow = firstRow
        , theTextPointColumn = firstColumn
        }
    }
  unCharIndex = unwrapIndex . fromIndex :: CharIndex -> Int
  TextPoint{theTextPointRow=firstRow, theTextPointColumn=firstColumn} = theTextRangeStart range
  TextPoint{theTextPointRow=lastRow , theTextPointColumn=lastColumn } = theTextRangeEnd   range
  loop stack fold (Parser p) parst =
    get >>= \ editor ->
    liftIO
    ( runStateT
      ( runStringParser
        (runReaderT p editor)
        parst
      )
      fold
    ) >>= \ ((result, parst), fold) ->
    case result of
      NoMatch         -> pure $ stack . ((ParserResult parst fold $ Left "") :) $ []
      ParseError err  -> pure $ stack . ((ParserResult parst fold $ Left err) :) $ []
      ParseOK    a    ->
        if doRepeat then
          loop (stack . ((ParserResult parst fold $ Right a) :)) fold p0 parst
        else
          pure $ stack . ((ParserResult parst fold $ Right a) :) $ []
      ParseWait  p    ->
        if theParserLineIndex parst >= lastRow then
          if theParserIsEOF parst then
            pure $ stack . ((ParserResult parst fold $ Left "unexpected EOF") :) $ []
          else
            loop stack fold (Parser $ lift p) (parst{ theParserIsEOF = True })
        else
          let nextRow = theParserLineIndex parst + 1 in
          getLineIndex nextRow >>= \ line ->
          loop stack fold (Parser $ lift p) $
          parst
          { theParserString =
              if nextRow < lastRow then line else
              cutFromEnd (unCharIndex lastColumn) line
          , theParserLineIndex = nextRow
          , theParserIndex = 0
          , theParserIsEOF = nextRow >= lastRow
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
