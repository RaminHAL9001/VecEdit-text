-- | There is also the 'IOByteStream' APIs for  constructing strings from file 'Handle's as a stream
-- of bytes encoding a  string of UTF-8 characters, and the 'EditLine'  API for constructing strings
-- using a 'GapBuffer'.
module VecEdit.Text.Internal
  ( -- ** String data types
    StringData(..), ByteVector(..), Char8, toChar8Lossy, fromChar8,
    CharVector, byteVectorSize,
    -- *** Lines of text without line breaks
    TextLine, theTextLineBreak, textLineData, textLineString, theTextLineTags,
    textLineTags, textLineBreakSymbol, textLineNull,
    emptyTextLine, textLineChomp, textLineContentLength,
    -- ** Folding over strings
    FoldableString(..), StringLength(..),
    CharStreamable(..), CharStream,
    getCharStream, stepCharStream, getCharStreamCount, charStreamAppend,
    indexFoldCharStream, foldCharStream, indexFoldStreamable, foldStreamable,
    charStreamSetChar, charStreamSetEnd, charStreamUTF8Bytes, charStreamVector,
    -- ** Indexing strings
    IndexableString(..), IOIndexableString(..), CuttableString(..),
    FromStringData(..), ToStringData(..), tryConvertString, convertString,
    textFromString, bytesFromString, lazyBytesFromString, byteVecFromString, uvecFromString,
    hPutFoldable, putFoldable,
    -- ** Line Editing
    LineEditor
      ( liftEditLine, countLines, pushLine, popLine, insertLineAtEnd, insertLineHere,
        copyLine, cutLine, copyBuffer, clearBuffer, shiftCursor,
        pushChar, popChar, tryPopChar, countChars
      ),
    defaultEditLineLiftResult,
    insertString, insert, joinLine, newline, lineBreak, copyBufferClear,
    EditLine(..), LineBuffer, EditLineState, newEditLineState, runEditLine, streamEditor,
    EditLineResult(..), editLineUTFWeight, onEditLineState, catchEditLineResult,
    maxCharIndex, minCharIndex, columnNumber,
    editLineTokenizer, editLineBreakSymbol, editLineLiftResult,
    -- ** Decoding byte streams
    IOByteStream(..), EditorStream,
    newEditorStream, streamByteString, streamLazyByteString,
    -- *** Iterating over decoded strings
    EditStream(..), EditStreamState, HaltEditStream, HandleUTFDecodeError,
    newEditStreamState, editStreamState, runEditStream,
    hFoldLines, streamFoldLines, hSetByteStreamMode,
    byteStreamDecode, hEditStreamBufferSize,
    -- *** UTF-8 Decoding
    UTF8Decoder(..), utf8Decoder, utf8DecoderPushByte,
    UTF8DecodeTakeChar, UTF8DecodeStep, UTF8DecodeTakeError, utf8ErrorDecompose,
    module Data.String
  ) where

import VecEdit.Types
  ( VectorIndex, VectorSize, rangeStart, rangeLength,
    ToIndex(toIndex), GaplessIndex(..), CharIndex, CharBufferSize,
    RelativeIndex, RelativeDirection(..), EditTextError(..), TextPrimOpError(PopItem),
  )

import VecEdit.Text.Line.Break
  ( LineBreakSymbol(..), LineBreakerState(..), lineBreakSize, lineBreak_LF
  )
import VecEdit.Text.TokenizerTable (TokenizerTable, tokTableLookup)
import VecEdit.Vector.Editor
  ( EditorState, runEditor, newCurrentBuffer, newEditorState,
    currentCursor, currentBuffer, currentRange,
  )
import qualified VecEdit.Vector.Editor as Ved
import qualified VecEdit.Vector.Editor.GapBuffer as GapBuf
import VecEdit.Vector.Editor.GapBuffer
  ( GapBuffer, GapBufferState, runGapBuffer,
    gapBufferEditorState, gapBufferVector, gapBuffer3Slice
  )

import Control.Arrow ((***), (>>>))
import Control.Lens
  ( Lens', lens, use, (^.), (+~), (.~), (.=), (%=), (+=), (-=)
  )
import Control.Monad (when, unless, ap, (>=>))
import Control.Monad.Cont (MonadCont(..), ContT(..), callCC, runContT)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask), ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..), State, runState)
import Control.Monad.Trans (lift)

import qualified Codec.Binary.UTF8.Generic as UTF8
import Codec.Binary.UTF8.Generic (UTF8Bytes(..))
import Codec.Binary.UTF8.String (encodeChar)

import Data.Bits ((.&.), (.|.), setBit, testBit, shift, complement)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as CBytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.ByteString.Builder as BuildBytes
import Data.ByteString.Char8 (ByteString)
import Data.Char (chr, ord, isAscii)
import Data.String (IsString(..))
import qualified Data.String.UTF8 as UTF8String
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as BuildText
import qualified Data.Vector.Unboxed as UVec
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as UMVec
import Data.Void (Void, vacuous)

import Data.Word (Word8, Word16, Word32)

import System.IO
  ( Handle, hSetEncoding, char8, hSetBuffering, BufferMode(NoBuffering),
    hPutChar, hGetChar, hIsEOF, stdout,
  )

----------------------------------------------------------------------------------------------------

type UVector = UVec.Vector
type CharVector = UVector Char

-- | This is an unsigned 8-bit word, enough to  hold a single ASCII value. This is much smaller than
-- Haskell's built-in 'Char' which supports the UTF character set.
type Char8 = Word8

newtype ByteVector = ByteVector { unwrapByteVector :: UVector Char8 }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show ByteVector where
  show (ByteVector vec) = show $ fromChar8 <$> UVec.toList vec

-- | The size of the 'ByteVector'
byteVectorSize :: ByteVector -> VectorSize
byteVectorSize (ByteVector str) = UVec.length str

-- | Convert a 'Char' to  a 'Char8' by throwing away the upper bits of the  'ord' of the 'Char'. You
-- lose information when you do this, so avoid doing  it. The functionality in this module do a good
-- job of keeping track of converting between  'Char' sequence encodings and byte sequence encodings
-- so as not to lose information.
toChar8Lossy :: Char -> Char8
toChar8Lossy = fromIntegral . ord

-- | Convert a ASCII Char8 value encoded as a 'Char8' into a full (wide) 'Char' value.
fromChar8 :: Char8 -> Char
fromChar8 = chr . fromIntegral

instance UTF8Bytes ByteVector Int where
  bsplit i (ByteVector str) = ByteVector *** ByteVector $ UVec.splitAt i str
  bdrop i (ByteVector str) = ByteVector $ UVec.drop i str
  buncons (ByteVector str) =
    if UVec.null str then Nothing else
    Just (str ! 0, ByteVector $ UVec.tail str)
  elemIndex w (ByteVector str) = UVec.findIndex (== w) str
  empty = ByteVector UVec.empty
  null (ByteVector str) = UVec.null str
  pack = ByteVector . UVec.fromList
  tail (ByteVector str) = ByteVector $ UVec.tail str

----------------------------------------------------------------------------------------------------

-- | This data type encompasses all of Haskell's major immutable text/string data types.
--
-- Note that this data  type does instantiate the 'Eq' typeclass, but two  strings are only equal if
-- they have the exact same encoding. So @'(==)'@  on 'StringData' is __NOT__ the same as testing if
-- two strings are equal, it is testing if two  strings are equal and have the same encoding. So the
-- semantics of  the 'StringData' data type  is an expression  of both a  string and also how  it is
-- formatted in memory.
data StringData
  = StringUndefined
  | StringText    !Strict.Text
  | StringBytes   !ByteString
  | StringByteVec !ByteVector
  | StringVector  !(UVector Char)
  deriving Eq

instance Show StringData where
  show = \ case
    StringUndefined -> "nil"
    StringText    a -> show a
    StringBytes   a -> show (UTF8String.fromRep a)
    StringByteVec a -> show (UTF8String.fromRep a)
    StringVector  a -> show a

instance IsString StringData where { fromString = toStringData . Just; }

-- |  Finding the  number of  'Char' values  encoded into  a string-like  data structure.  Functions
-- instantiating this typeclass which may require scanning the entire string.
class StringLength   t where { stringLength :: t -> Int; }

-- | Data structures that can be converted from a 'StringData'.
class FromStringData t where { fromStringData :: StringData -> Maybe t; }

-- | Data structures that can be converted to a 'StringData'.
class ToStringData   t where { toStringData   :: Maybe t -> StringData; }

-- | Convert from one string  type to another. Be warned that this may  cause space leaks when using
-- the usual  'String' data type, as  all 'String' values  are first converted to  a @('UVec.Vector'
-- 'Char')@  type  first,  which consumes  the  entire  'String'  and  store it  in  memory,  before
-- re-encoding it as some other string type.
tryConvertString :: (ToStringData a, FromStringData b) => a -> Maybe b
tryConvertString = fromStringData . toStringData . Just

-- |  An   non-total  version   of  'tryConvertString',   i.e.  it  evaluates   to  an   'error'  if
-- 'tryConvertString' evaluates to 'Nothing'.
convertString :: (ToStringData a, FromStringData b) => a -> b
convertString =
  maybe (error "convertString: fromStringData evaluates to undefined") id .
  tryConvertString

instance StringLength String    where { stringLength = length; }
instance StringLength ByteVector  where { stringLength = UTF8.length; }
instance StringLength Strict.Text   where { stringLength = Strict.length; }
instance StringLength (UVector Char)  where { stringLength = UVec.length; }
instance StringLength ByteString where { stringLength = UTF8.length; }
instance StringLength StringData where
  stringLength = \ case
    StringUndefined -> 0
    StringText    a -> stringLength a
    StringBytes   a -> stringLength a
    StringByteVec a -> stringLength a
    StringVector  a -> stringLength a

instance ToStringData String where
  toStringData =
    maybe
    StringUndefined
    (\ case
      ""  -> StringByteVec $ ByteVector UVec.empty
      str -> StringVector $ UVec.fromList str
    )
instance ToStringData ByteString where
  toStringData = maybe StringUndefined StringBytes
instance ToStringData (UVec.Vector Char) where
  toStringData = maybe StringUndefined StringVector
instance ToStringData ByteVector where { toStringData = maybe StringUndefined StringByteVec; }
instance ToStringData Strict.Text  where { toStringData = maybe StringUndefined StringText; }
instance ToStringData StringData     where { toStringData = maybe StringUndefined id; }

instance FromStringData String where
  fromStringData = \ case
    StringUndefined -> Nothing
    StringText    a -> Just $ Strict.unpack a
    StringBytes   a -> Just $ fromChar8 <$> Bytes.unpack a
    StringByteVec a -> Just $ fromChar8 <$> UVec.toList (unwrapByteVector a)
    StringVector  a -> Just $ UVec.toList a

instance FromStringData Strict.Text where
  fromStringData = \ case
    StringUndefined -> Nothing
    StringText    a -> Just a
    StringBytes   a -> Just $ textFromString a
    StringByteVec a -> Just $ textFromString a
    StringVector  a -> Just $ textFromString a

instance FromStringData ByteString where
  fromStringData = \ case
    StringUndefined -> Nothing
    StringText    a -> Just $ bytesFromString a
    StringBytes   a -> Just a
    StringByteVec a -> Just $ bytesFromString a
    StringVector  a -> Just $ bytesFromString a

instance FromStringData ByteVector where
  fromStringData = \ case
    StringUndefined -> Nothing
    StringText    a -> Just $ byteVecFromString a
    StringBytes   a -> Just $ byteVecFromString a
    StringByteVec a -> Just a
    StringVector  a -> Just $ byteVecFromString a

instance FromStringData (UVector Char) where
  fromStringData = \ case
    StringUndefined -> Nothing
    StringText    a -> Just $ uvecFromString a
    StringBytes   a -> Just $ uvecFromString a
    StringByteVec a -> Just $ uvecFromString a
    StringVector  a -> Just a

instance FromStringData StringData where
  fromStringData = \ case
    StringUndefined -> Nothing
    str             -> Just str

----------------------------------------------------------------------------------------------------

class FoldableString str where
  foldString :: (fold -> Char -> fold) -> fold -> str -> fold

instance FoldableString [Char] where
  foldString = foldl

instance FoldableString Strict.Text where
  foldString = Strict.foldl

instance FoldableString ByteString where
  foldString f accum str = case UTF8.decode str of
    Nothing     -> accum
    Just (c, n) -> foldString f (f accum c) (UTF8.drop n str)

instance FoldableString LazyBytes.ByteString where
  foldString f accum str = case UTF8.decode str of
    Nothing     -> accum
    Just (c, n) -> foldString f (f accum c) (UTF8.drop n str)

instance FoldableString ByteVector where
  foldString f accum = foldl f accum . UTF8.toString

instance FoldableString (UVector Char) where
  foldString = UVec.foldl

instance FoldableString StringData where
  foldString f fold = \ case
    StringUndefined   -> fold
    StringText    str -> foldString f fold str
    StringBytes   str -> foldString f fold str
    StringByteVec str -> foldString f fold str
    StringVector  str -> foldString f fold str

-- | Use 'FoldableString' to construct a 'Strict.Text' value.
textFromString :: FoldableString str => str -> Strict.Text
textFromString =
  Lazy.toStrict .
  BuildText.toLazyText .
  foldString (\ str -> (str <>) . BuildText.singleton) mempty

-- | Use 'FoldableString' to construct a __lazy__ UTF8-encoded 'LazyBytes.ByteString' value.
lazyBytesFromString :: FoldableString str => str -> LazyBytes.ByteString
lazyBytesFromString = 
  BuildBytes.toLazyByteString .
  foldString (\ str -> (str <>) . BuildBytes.charUtf8) mempty

-- | Use 'FoldableString' to construct a __strict__ UTF8-encoded 'ByteString' value.
bytesFromString :: FoldableString str => str -> ByteString
bytesFromString =
  LazyBytes.toStrict .
  lazyBytesFromString

-- | Use 'FoldableString' to construct a UTF8-encoded 'ByteString' value.
byteVecFromString :: FoldableString str => str -> ByteVector
byteVecFromString =
  ByteVector .
  UVec.fromList .
  foldString (\ str -> (str <>) . encodeChar) []

uvecFromString :: FoldableString str => str -> UVector Char
uvecFromString =
  UVec.fromList .
  ($ []) .
  foldString (\ str c -> str . (c :)) id

-- | Print any 'FoldableString' to the given file 'Handle'.
hPutFoldable :: FoldableString str => Handle -> str -> IO ()
hPutFoldable h = foldString (\ f c -> f >> hPutChar h c) (pure ())

-- | Like 'hPutFoldable', but uses 'stdout' as the file handle.
putFoldable :: FoldableString str => str -> IO ()
putFoldable = hPutFoldable stdout

----------------------------------------------------------------------------------------------------

-- | Similar to  a 'String', but optimized  for parsing. Use the 'toCharStream'  function to convert
-- some kind of string into a 'CharStream'.
data CharStream
  = CharStream
    { charStreamBits :: !Word32
    , getCharStreamCount :: !Int
      -- ^ Get the number of characters that have been consumed so far.
    , stepCharStream :: CharStream
      -- ^ Advance the 'CharStream' by 1 character.
    }

instance Semigroup CharStream where
  a <> b = case getCharStream a of
    Nothing -> onCharStreamCount (+ (getCharStreamCount a)) b
    Just{}  -> stepCharStream a <> b

instance Monoid CharStream where
  mempty = charStreamSetEnd 0
  mappend = (<>)

instance Show CharStream where
  show st =
    "(char-stream :index " <>
    show (getCharStreamCount st) <>
    ( case getCharStream st of
        Nothing -> " :end"
        Just  c -> " :next " <> show c
    ) <> ")"

onCharStreamCount :: (Int -> Int) -> CharStream -> CharStream
onCharStreamCount f st = st
  { getCharStreamCount = f $! getCharStreamCount st
  , stepCharStream = onCharStreamCount f $ stepCharStream st
  }

-- | Get the next 'Char' in the 'CharStream', along with the next 'CharStream' function.
getCharStream :: CharStream -> Maybe Char
getCharStream st =
  if testBit (charStreamBits st) 31 then Nothing else
  Just $ chr $ fromIntegral $ charStreamBits st

-- | Put more characters into a 'CharStream'
charStreamAppend :: CharStreamable string => CharStream -> string -> CharStream
charStreamAppend = toCharStream . getCharStreamCount

-- | Fold an accumulating  function over all characters in a 'CharStream'  to produce an accumulated
-- value of type @accum@.  The accumulating function also takes the index  value returned by calling
-- 'getCharStreamCount' on the 'CharStream'
indexFoldCharStream :: (Int -> Char -> accum -> accum) -> CharStream -> accum -> accum
indexFoldCharStream f st accum = case getCharStream st of
  Nothing -> accum
  Just c  ->
    indexFoldCharStream f
    (stepCharStream st)
    (f (getCharStreamCount st) c accum)

-- | Like 'indexFoldCharStream' but ignores the index value.
foldCharStream :: (Char -> accum -> accum) -> CharStream -> accum -> accum
foldCharStream = indexFoldCharStream . const

-- |  Runs  'foldCharStream' on  a  @string@  that  instantiates  the 'CharStreamable'  class.  This
-- basically calls 'foldCharStream' on the result of 'toCharStream'.
indexFoldStreamable
  :: CharStreamable string
  => (Int -> Char -> accum -> accum)
  -> Int -> string -> accum -> accum
indexFoldStreamable f i = indexFoldCharStream f . toCharStream i

-- | Same as 'indexFoldStreamable' but ignores the index value.
foldStreamable
  :: CharStreamable string
  => (Char -> accum -> accum)
  -> string -> accum -> accum
foldStreamable f = foldCharStream f . toCharStream 0

-- | Construct  a 'CharStream' passing  the current 'Char',  and a lazily-evaluated  'CharStream' to
-- produce the  next character, which  might be  'charStreamFinal' if the  given 'Char' is  the last
-- one. The 'Int'  argument is the number of  characters that have been consumed, so  it is expected
-- that you use this function in a loop in which you are keeping track of the number of characters.
charStreamSetChar :: Int -> Char -> CharStream -> CharStream
charStreamSetChar count c ~next = CharStream
  { charStreamBits = fromIntegral $ ord c
  , getCharStreamCount = count
  , stepCharStream = next
  }

-- | Construct an  empty 'CharStream' in which 'getCharStream' always  produces 'Nothing'. Like with
-- 'charStreamSetChar', it is expected that you use this function in a loop in which you are keeping
-- track of the number of characters.
charStreamSetEnd :: Int -> CharStream
charStreamSetEnd i = CharStream
  { charStreamBits = setBit 0 31
  , getCharStreamCount = i
  , stepCharStream = charStreamSetEnd i
  }

-- | Constructs a 'CharStream'  from UTF8-encoded characters that have been stored  in either a lazy
-- 'LazyBytes.ByteString'   or  strict   'ByteString'.  This   function  is   used  to   instantiate
-- 'toCharStream'  for   both  lazy  'LazyBytes.ByteString's   and  strict  'ByteString's,   so  the
-- 'toCharStream' function is a more general type of this function.
charStreamUTF8Bytes :: UTF8Bytes bytestring s => Int -> bytestring -> CharStream
charStreamUTF8Bytes i str = case UTF8.decode str of
  Nothing     -> charStreamSetEnd i
  Just (c, n) ->
    charStreamSetChar i c $
    (charStreamUTF8Bytes $! i + 1) $
    UTF8.drop n str

-- | Constructs a 'CharStream' from any unboxed 'UVec.Vector'.
charStreamVector :: UMVec.Unbox c => (c -> Char) -> Int -> UVec.Vector c -> CharStream
charStreamVector chr count str = loop count 0 where
  loop count i =
    if i >= UVec.length str then charStreamSetEnd i else
    charStreamSetChar count
    (chr $ UVec.unsafeIndex str i)
    ((loop $! count + 1) $! i + 1)

-- | This is a typeclass of string types  that can produce streams of characters. In particular this
-- is useful for parsing, and is used by the "VecEdit.Text.Parser" module.
class CharStreamable str where

  -- | Convert a string  to a 'CharStream'. Pass the  initial 'Int' value, which should be  0 if you
  -- are initializing this 'CharStream' at the beginning of the string.
  toCharStream :: Int -> str -> CharStream

instance CharStreamable CharStream where
  toCharStream i st = onCharStreamCount (+ i) $ st{ getCharStreamCount = 0 }

instance CharStreamable String where
  toCharStream i = \ case
    ""   -> charStreamSetEnd i
    c:cx -> charStreamSetChar i c $ (toCharStream $! i + 1) cx

instance CharStreamable ByteString where
  toCharStream = charStreamUTF8Bytes

instance CharStreamable LazyBytes.ByteString where
  toCharStream = charStreamUTF8Bytes

instance CharStreamable Strict.Text where
  toCharStream i str =
    if Strict.null str then charStreamSetEnd i else
    charStreamSetChar i (Strict.head str) $
    (toCharStream $! i + 1) $
    Strict.tail str

instance CharStreamable ByteVector where
  toCharStream i = charStreamVector fromChar8 i . unwrapByteVector

instance CharStreamable (UVector Char) where
  toCharStream = charStreamVector id

instance CharStreamable StringData where
  toCharStream i = \ case
    StringUndefined -> charStreamSetEnd i
    StringText    str -> toCharStream i str
    StringBytes   str -> toCharStream i str
    StringByteVec str -> toCharStream i str
    StringVector  str -> toCharStream i str

instance CharStreamable TextLineData where
  toCharStream i = \ case
    TextLineEmpty       -> charStreamSetEnd i
    TextLineBytes   str -> toCharStream i str
    TextLineChars _ str -> toCharStream i str

instance CharStreamable (TextLine tags) where
  toCharStream i = toCharStream i . textLineString

----------------------------------------------------------------------------------------------------

-- | A class  of string data types  that can be indexed,  retriving a 'Char' at  some 'Int' position
-- from the start of the @str@ string. As a law,  instances of this type class must be able to index
-- a @str@ data type in O(1) time.
class IndexableString str where
  charAtIndex :: str -> Int -> Char

instance IndexableString (UVector Char) where
  charAtIndex = (!)

instance IndexableString ByteString where
  charAtIndex str = CBytes.index str

instance IndexableString ByteVector where
  charAtIndex (ByteVector str) = fromChar8 . (str !)

instance IndexableString StringData where
  charAtIndex str i = case stringDataGetChar str i of
    Left err -> error $ Strict.unpack err
    Right  c -> c

-- | Like 'IndexableString' but must be indexed in the IO monad
class IOIndexableString str where
  charAtIndexIO :: str -> Int -> IO Char

instance IOIndexableString (UMVec.IOVector Char) where
  charAtIndexIO = UMVec.read

instance IOIndexableString (UMVec.IOVector Char8) where
  charAtIndexIO str = fmap fromChar8 . UMVec.read str

instance IOIndexableString (UVector Char) where
  charAtIndexIO str = pure . charAtIndex str

instance IOIndexableString ByteVector where
  charAtIndexIO str = pure . charAtIndex str

instance IOIndexableString StringData where
  charAtIndexIO str = pure . charAtIndex str

----------------------------------------------------------------------------------------------------

-- | This data type contains a string that is guarnateed to contain exactly zero or exactly one line
-- breaking  characters, and  the  break  is guaranteed  to  exist  only at  the  end  of the  line.
-- 'TextLine's  are constructed  by  functions  of the  'EditLine'  type,  or the  'streamFoldLines'
-- function and it's associates.
--
-- A  'TextLine' by  default will  encode strings  as 'ByteVector's,  but will  automatically detect
-- non-ASCII characters -- any 'Char' that does not  satisfy the 'isAscii' predicate, where @ord c >
-- 127@. If  non-ASCII characters exist,  the internal buffer is  converted from 'ByteVector'  to an
-- unboxed @('Vector' 'Char')@, a.k.a. 'CharVector'.
--
-- The @tags@ type variable  allows you to store annotation data along with  this string, which is a
-- feature used more by the "VecEdit.Text.Editor" module for storing intermediate parser states when
-- analyzing source code, or attaching text properties like colors or font faces.
--
-- 'TextLine' should  not be used  as a  string, it contains  additional information about  the line
-- break such  that the internal  representation, which is of  type 'StringData', might  be slightly
-- different from the original string that encoded  it. The internal represntation always drops line
-- breaking characters, even if the original string encoding used a @"\\n\\r"@ or @"\\r\\n"@ pair of
-- characters to encode the line break. The line break is stored as 'LineBreakSymbol' retrieved with
-- the 'theTextLineBreak' function.
--
-- The originally encoded string  value can still be retrieved using  'foldString', which folds over
-- the original  line breaking  characters at  the end of  the string  (if any).  The 'stringLength'
-- function also returns  the "true" length of  the original string encoding,  counting 2 characters
-- for a line break if  @"\\n\\r"@ or @"\\r\\n"@ were used to encode the  line break. For the length
-- of the internal representation, without the line breaks, use 'textLineContentLength'.
data TextLine tags
  = TextLine
    { theTextLineBreak :: !LineBreakSymbol
    , theTextLineData  :: !TextLineData
    , theTextLineTags  :: !(Maybe tags)
    }
  deriving (Eq, Functor)

data TextLineData
  = TextLineEmpty
  | TextLineBytes !ByteVector
  | TextLineChars !Int !CharVector
  deriving (Eq, Ord)

instance Show TextLineData where
  show = \ case
    TextLineEmpty         -> "\"\""
    TextLineBytes   vec -> show vec
    TextLineChars _ vec -> show $ UVec.toList vec

instance Show tags => Show (TextLine tags) where
  show = showTextLine show

instance StringLength TextLineData where
  stringLength = \ case
    TextLineEmpty         -> 0
    TextLineBytes   vec -> stringLength vec
    TextLineChars _ vec -> stringLength vec

instance StringLength (TextLine tags) where
  stringLength (TextLine{theTextLineBreak=lbrk,theTextLineData=str}) =
    stringLength str + lineBreakSize lbrk

instance FoldableString TextLineData where
  foldString f fold = \ case
    TextLineEmpty       -> fold
    TextLineBytes   str -> foldString f fold str
    TextLineChars _ str -> foldString f fold str

instance FoldableString (TextLine tags) where
  foldString f fold str =
    foldl f
    (foldString f fold $ theTextLineData str)
    (show $ theTextLineBreak str)

showTextLine :: (tags -> String) -> TextLine tags -> String
showTextLine showTags (TextLine{theTextLineBreak=lbrk,theTextLineData=str,theTextLineTags=tags}) =
  "(" <> show str <> " " <> show (show lbrk) <>
  maybe "" ((" :tags " <>) . showTags) tags <> ")"

textLineBytes :: UVector Char8 -> TextLineData
textLineBytes = TextLineBytes . ByteVector

textLineContentLength :: TextLine tags -> VectorSize
textLineContentLength = stringLength . theTextLineData

-- | Produce a count of the number of non-ASCII characters there are in this 'TextLine'. A non-ASCII
-- character is any 'Char' for which the 'isAscii' predicate is 'False'.
textLineUTFWeight :: TextLine tags -> Int
textLineUTFWeight txt = case theTextLineData txt of
  TextLineEmpty       -> 0
  TextLineBytes{}   -> 0
  TextLineChars w _ -> w

-- |  Retrieve  the  internal  'TextLine'  data,  which   does  not  contain  any  line  break.  Use
-- 'convertString' or 'foldString' to produce the original string with line breaks.
textLineData :: TextLine tags -> StringData
textLineData txt = case theTextLineData txt of
  TextLineEmpty       -> StringUndefined
  TextLineBytes   vec -> StringByteVec vec
  TextLineChars _ vec -> StringVector vec

-- | Convert a 'TextLine' to a 'String' terminated by the correct 'LineBreakSymbol'.
textLineString :: TextLine tags -> String
textLineString txt =
  foldString (\ stack c -> stack . (c :)) id txt $
  show $ theTextLineBreak txt

textLineTags :: Lens' (TextLine tags) (Maybe tags)
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

textLineBreakSymbol :: Lens' (TextLine tags) LineBreakSymbol
textLineBreakSymbol = lens theTextLineBreak $ \ a b -> a{ theTextLineBreak = b }

textLineNull :: TextLine tags -> Bool
textLineNull = theTextLineData >>> \ case { TextLineEmpty -> True; _ -> False; }

emptyTextLine :: TextLine tags
emptyTextLine =
  TextLine
  { theTextLineBreak = NoLineBreak
  , theTextLineData  = TextLineEmpty
  , theTextLineTags  = Nothing
  }

-- | If the 'TextLine' has a terminating line break, remove the line break.
textLineChomp :: TextLine tags -> TextLine tags
textLineChomp str = str{ theTextLineBreak = NoLineBreak }

-- | Similar to 'charAtIndex', but may fail (return Nothing) if indecing an 'undefinedTextLine'.
stringDataGetChar :: StringData -> VectorIndex -> Either Strict.Text Char
stringDataGetChar str i = case str of
  StringText    str -> Right $ Strict.index str i
  StringBytes   str -> Right $ charAtIndex str i
  StringByteVec str -> Right $ charAtIndex str i
  StringVector  str -> Right $ charAtIndex str i
  StringUndefined   ->
    Left $ Strict.pack $
    "evaluated (charAtIndex " <> show i <> ") on undefined string"

----------------------------------------------------------------------------------------------------

-- | This class defines  functions for taking a part of a string  from a given 'VectorIndex', toward
-- the  start or  end of  a  string. If  the given  'VectorIndex' is  too  big, an  empty string  is
-- returned. If the given 'VectorIndex' is too small, the string is returned unmodified.
class CuttableString str where
  -- | Remove characters from the start of the string up to the given 'VectorIndex'.
  cutFromStart :: VectorIndex -> str -> str
  -- | Remove characters from the end of the string back to the given 'VectorIndex'.
  cutFromEnd :: VectorIndex -> str -> str

instance CuttableString ByteString where
  cutFromStart i = fst . CBytes.splitAt i
  cutFromEnd   i = snd . CBytes.splitAt i

instance CuttableString ByteVector where
  cutFromStart i (ByteVector str) = ByteVector $ uvecCutFromStart i str
  cutFromEnd   i (ByteVector str) = ByteVector $ uvecCutFromEnd   i str

instance CuttableString (UVector Char) where
  cutFromStart = uvecCutFromStart
  cutFromEnd   = uvecCutFromEnd

instance CuttableString Strict.Text where
  cutFromStart = Strict.drop
  cutFromEnd = Strict.dropEnd

instance CuttableString StringData where
  cutFromStart i = \ case
    StringUndefined   -> StringUndefined
    StringText    str -> StringText    $ cutFromStart i str
    StringBytes   str -> StringBytes   $ cutFromStart i str
    StringByteVec str -> StringByteVec $ cutFromStart i str
    StringVector  str -> StringVector  $ cutFromStart i str
  cutFromEnd i = \ case
    StringUndefined   -> StringUndefined
    StringText    str -> StringText    $ cutFromEnd i str
    StringBytes   str -> StringBytes   $ cutFromEnd i str
    StringByteVec str -> StringByteVec $ cutFromEnd i str
    StringVector  str -> StringVector  $ cutFromEnd i str

uvecCutFromStart :: UVec.Unbox c => VectorIndex -> UVector c -> UVector c
uvecCutFromStart = UVec.drop

uvecCutFromEnd :: UVec.Unbox c => VectorIndex -> UVector c -> UVector c
uvecCutFromEnd i vec = UVec.take (UVec.length vec - i) vec

----------------------------------------------------------------------------------------------------

-- |  This  module  provides  the  'EditLine'  function   type  and  several  useful  APIs  of  this
-- type. However, it  might be useful to use  these same APIs to produce functions  types other than
-- 'EditLine', for example 'VecEdit.Text.Editor.EditText' in the "VecEdit.Text.Editor" module.
--
-- The minimal complete definition is 'liftEditLine'. But every kind of editor can plauisbly provide
-- completely different functionality for each of these  APIs depending on the memory model used for
-- line buffering.
--
-- The biggest  difference between various  instances of 'LineEditor' is  the memory model  used for
-- line buffering.  The most  primitive instance  of 'LineEditor' is  the 'EditLine'  function type,
-- which does no line buffering, only character buffering. The 'EditStream' function type provides a
-- 'Before'   stack   and  'After'   stack   but   no  random   access   of   stack  elements.   The
-- 'VecEdit.Text.Editor.EditText'        function       provides        a       random        access
-- 'VecEdit.Vector.Editor.GapBuffer.GapBuffer' so  the cursor can  efficiently move anywhere  in the
-- buffer at any time.
class LineEditor (editor :: * -> * -> *) where

  -- | Lift an 'EditLine' function into the @editor@ function.
  liftEditLine :: Monad (editor tags) => EditLine tags a -> editor tags a

  -- | Lift a  given 'EditLineResult'  result into  the @editor@ monad.  If the  'EditLineResult' is
  -- @('EditLineOK' a)@, this is the same as @('return' a)@ or @('pure' a)@. If the 'EditLineResult'
  -- is @('EditLineFail' err)@, this  is the same as @@'throwError' err)@.  You can instantiate this
  -- function with 'defaultEidtLineLiftResult' if your  @editor@ monad instantiates the 'MonadError'
  -- class over the 'EditTextError' type.
  editLineLiftResult :: EditLineResult editor tags a -> editor tags a

  -- | Count the number of lines 'Before' or 'After' the cursor.
  --
  -- The  most  primitive instance  of  'LineEditor',  the  'EditLine' function  type,  instantiates
  -- 'countLines' with a function that simply always returns a value of zero. But other instances of
  -- 'LineEditor'  can provide  instances  of  'countLines' that  can  return  non-zero values.  For
  -- example, the 'VecEdit.Text.Editor.EditText' provides provides the ability to edit many lines of
  -- text in a buffer,  as well move around randomly in the buffer,  so the 'countLines' instance is
  -- of more consequence for the 'VecEdit.Text.Editor.EditText' function type.
  countLines :: Monad (editor tags) => editor tags VectorSize
  countLines = pure 0

  -- | Ask the  evaluation context  to store a  'TextLine' 'Before' (above)  or 'After'  (below) the
  -- current line.
  pushLine :: Monad (editor tags) => RelativeDirection -> TextLine tags -> editor tags ()
  pushLine = (.) liftEditLine . pushLine

  -- | Ask the evaluation  context to remove a  'TextLine' from above ('Before')  or below ('After')
  -- the cursor.
  popLine :: Monad (editor tags) => RelativeDirection -> editor tags (TextLine tags)
  popLine = liftEditLine . popLine

  -- | Insert a 'TextLine' into the current buffer at the either end of the buffer (depending on the
  -- 'RelativeDirection').  This  function is  intended  to  be used  as  a  primitive step  in  the
  -- 'joinLine' function, the difference  between this and 'joinLine' is that  it joins an arbitrary
  -- given 'TextLine', rather  than the next or previous  line in the editor context.  See also: the
  -- 'insertLineHere' function inserts a 'TextLine' at the cursor position.
  --
  -- If inserting 'Before' the given 'TextLine' has it's line break removed before inserting. If
  -- inserting 'After', and the given 'TextLine' is terminated with a line break, this line break
  -- replaces the current line break.
  --
  -- If necssary, you should use the 'textLineTags' lens to update the @tags@ after this function has
  -- returned.
  insertLineAtEnd :: Monad (editor tags) => RelativeDirection -> TextLine tags -> editor tags ()
  insertLineAtEnd = (.) liftEditLine . insertLineAtEnd

  -- | Insert a 'TextLine' at the current cursor position 'Before' or 'After' the cursor. See also:
  -- the 'insertLineAtEnd' function.
  insertLineHere
    :: (Monad (editor tags), LineEditor editor)
    => RelativeDirection -> TextLine tags -> editor tags ()
  insertLineHere = (.) liftEditLine . insertLineHere

  -- | Insert the  given 'Char', if the  'Char' is the  ASCII "line feed" @'\\n'@  (a.k.a. @'\\LF'@)
  -- character, the 'EditLine' function halts in the @'EditLinePush' 'Before'@ state, returning with
  -- it a 'TextLine'.
  pushChar :: Monad (editor tags) => RelativeDirection -> Char -> editor tags ()
  pushChar = (.) liftEditLine . pushChar

  -- | This is the inverse  of 'pushChar', tries to remove a character from  'Before' or 'After' the
  -- cursor. If there are no characters left, 'joinLine' is evaluated and 'popChar' tries again.
  popChar :: Monad (editor tags) => RelativeDirection -> editor tags Char
  popChar = liftEditLine . popChar

  -- | Return the number of characters 'Before' or 'After' the cursor.
  countChars :: Monad (editor tags) => RelativeDirection -> editor tags VectorSize
  countChars = liftEditLine . countChars

  -- | Similar to  'popChar' but behaves differently  when the cursor is  at either end of  the line
  -- buffer and there  are no characters to be  popped: rather than evaluate 'joinLine'  when at the
  -- end of the 'EditLine' buffer, this function simply returns 'Nothing'.
  tryPopChar :: Monad (editor tags) => RelativeDirection -> editor tags (Maybe Char)
  tryPopChar = liftEditLine . tryPopChar

  -- | Construct a  'TextLine' from  the portion of  the 'EditLine' buffer  'Before' or  'After' the
  -- cursor,  clear  that  portion  of  the  'EditLine' buffer,  and  then  return  the  constructed
  -- 'TextLine'.
  copyLine :: Monad (editor tags) => RelativeDirection -> editor tags (TextLine tags)
  copyLine = liftEditLine . copyLine

  -- | Like 'copyLine', but clears the characters from the buffer after copying them.
  cutLine :: Monad (editor tags) => RelativeDirection -> editor tags (TextLine tags)
  cutLine = liftEditLine . cutLine

  -- | Copy the whole  'LineEditState' buffer into a  'TextString', but do not  clear the 'EditLine'
  -- buffer.
  copyBuffer :: Monad (editor tags) => editor tags (TextLine tags)
  copyBuffer = liftEditLine copyBuffer

  -- | Clear the content of the buffer.
  clearBuffer :: Monad (editor tags) => editor tags ()
  clearBuffer = liftEditLine clearBuffer

  -- | Shift the  cursor by  a 'RelativeIndex'.  Negative numbers indicate  motion in  the direction
  -- 'Before' the cursor, positive numbers indicate motion  in the direction 'After' the cursor. The
  -- number of  characters shifted is  returned. If the  cursor is  at the start  of the line  and a
  -- negative 'RelativeIndex'  is given 'Nothing' is  returned. If the cursor  is at the end  of the
  -- line and a positive 'RelativeIndex' is given 'Nothing' is returned.
  shiftCursor :: Monad (editor tags) => RelativeIndex -> editor tags RelativeIndex
  shiftCursor = liftEditLine . shiftCursor

defaultEditLineLiftResult
  :: ( Monad (editor tags)
     , MonadError EditTextError (editor tags)
     , LineEditor editor
     )
  => EditLineResult editor tags a -> editor tags a
defaultEditLineLiftResult = \ case
  EditLineOK a -> pure a
  EditLineFail err -> throwError err
  EditLinePush dir line next -> pushLine dir line >> next
  EditLinePop dir next -> popLine dir >>= next

----------------------------------------------------------------------------------------------------

-- | This is a function  type for editing a single line of text with  a unboxed mutable 'MVector' as
-- the character  buffer. You can  quickly insert characters  in @O(1)@ time,  and you can  move the
-- cursor in @O(n)@  time where @n@ is the  size of the cursor motion. This  monad behaves something
-- like an interruptable parsing monad (something like 'StringParser', or the attoparsec Parser). An
-- 'EditLine' function can pause itself on certain events, and be resumed.
--
-- One event that causes a  pause to occur is when a line breaking  character is inserted. When this
-- happens, inspect the 'EditLineResult', it may be waiting for the 'TextLine' it constructed on the
-- line  break  event   to  be  consumed  outside  of  the   'EditLine'  evaluation  context  before
-- resuming.  Typically  you  would  push  the  'TextLine'  into  another  data  structure,  like  a
-- 'VecEdit.Text.Editor.TextBuffer' that represents a larger string.
--
-- Another event that causes a pause to occur is when a line breaking character at the end of a line
-- is deleted. When this happens, the editor may need  to be filled with a line that comes after the
-- current line.
newtype EditLine tags a
  = EditLine
    { unwrapEditLine :: StateT (EditLineState tags) IO (EditLineResult EditLine tags a)
    }

-- | This is the stateful data availble to you when you use functions of the 'EditLine' type.
data EditLineState tags
  = EditLineState
    { theEditLineGapBuffer   :: !(GapBufferState UMVec.MVector Char)
      -- ^ This is the mutable text that can be edited.
    , theEditLineBreakSymbol :: !LineBreakSymbol
      -- ^ The default 'LineBreakSymbol' to use when constructing 'TextLine's.
    , theEditLineNonAsciiBefore :: !Int
      -- ^ Keeps track of the number of non-ASCII characters before the cursor.
    , theEditLineNonAsciiAfter :: !Int
      -- ^ Keeps track of the number of non-ASCII characters after the cursor.
    , theEditLineTokenizer :: !(TokenizerTable LineBreakSymbol LineBreakerState)
    }

-- | Synonym for 'EditLineState'. This name is more  descriptive for what the data type actually is:
-- the  data structure  that contains  the mutable  buffer of  characters used  for line  editing by
-- functions of the type 'EditLine'.
type LineBuffer tags = EditLineState tags

-- | This data  type is used to  control progress of the the  'EditLine' monad. It is  almost a free
-- monad in that it defines continuation functions to be called when certain events occur, and these
-- continuations are expected to be evaluated in some context external to the 'EditLine' monad.
data EditLineResult (editor :: * -> * -> *) tags a
  = EditLineOK a
    -- ^ The computation succeeded.
  | EditLineFail !EditTextError
    -- ^ The computation failed.
  | EditLinePush !RelativeDirection !(TextLine tags) (editor tags a)
    -- ^ A 'TextLine' was constructed an it needs to be pushed 'Before' or 'After' the cursror.
  | EditLinePop !RelativeDirection (TextLine tags -> editor tags a)
    -- ^ The line break at the end of the line was deleted and the line after the cursor needs to be
    -- copied into the buffer now.

instance Functor (editor tags) => Functor (EditLineResult editor tags) where
  fmap f = \ case
    EditLineOK            a -> EditLineOK $ f a
    EditLineFail err        -> EditLineFail err
    EditLinePush dir line a -> EditLinePush dir line $ fmap f a
    EditLinePop  dir      a -> EditLinePop dir $ fmap (fmap f) a

instance (Show a, Show tags) => Show (EditLineResult editor tags a) where
  show = showEditLineResult show show

instance Functor (EditLine tags) where
  fmap f (EditLine a) = EditLine $ fmap (fmap f) a

instance Monad (EditLine tags) where
  return = EditLine . return . EditLineOK
  (EditLine a) >>= f = EditLine $ a >>= \ case
    EditLineOK            a -> unwrapEditLine $ f a
    EditLineFail err        -> pure $ EditLineFail err
    EditLinePush dir line a -> pure $ EditLinePush dir line $ a >>= f
    EditLinePop  dir      a -> pure $ EditLinePop dir $ fmap (>>= f) a

instance Applicative (EditLine tags) where
  (<*>) = ap
  pure  = return

instance MonadIO (EditLine tags) where
  liftIO = EditLine . fmap EditLineOK . liftIO

instance MonadError EditTextError (EditLine tags) where
  throwError = EditLine . pure . EditLineFail
  catchError (EditLine f) catch =
    EditLine $ f >>= \ case
      EditLineFail err -> unwrapEditLine $ catch err
      result           -> pure result

instance MonadFail (EditLine tags) where
  fail = EditLine . pure . EditLineFail . EditTextFailed . Strict.pack

showEditLineResult :: (tags -> String) -> (a -> String) -> EditLineResult editor tags a -> String
showEditLineResult showTags showA = \ case
  EditLineOK            a -> "(ok" <> sp (showA a) <> ")"
  EditLineFail        err -> "(fail " <> show err <> ")"
  EditLinePush dir line _ -> "(push " <> show dir <> (' ' : showTextLine showTags line) <> ")"
  EditLinePop  dir      _ -> "(pop " <> show dir <> ")"
  where
    sp = \ case
      "" -> ""
      a  -> ' ' : a

----------------------------------------------------------------------------------------------------

instance LineEditor EditLine where

  -- liftEditLine :: EditLine tags a -> EditLine tags a
  liftEditLine = error
    "infinite loop: 'liftEditLine' evaluated in function context of type 'EditLine'"

  -- editLineLiftResult :: EditLineResult EditLine tags a -> EditLine tags a
  editLineLiftResult = EditLine . pure

  -- countLines :: EditLine tags VectorSize
  countLines = pure 0

  -- pushLine :: RelativeDirection -> TextLine tags -> EditLine tags ()
  pushLine dir = EditLine . pure . flip (EditLinePush dir) (pure ())

  -- popLine :: Monad (editor tags) => RelativeDirection -> editor tags (TextLine tags)
  popLine dir = EditLine $ pure $ EditLinePop dir pure

  -- insertLineAtEnd :: RelativeDirection -> TextLine tags -> EditLine tags ()
  insertLineAtEnd dir txt =
    case theTextLineData txt of
      TextLineEmpty -> pure ()
      TextLineChars _ vec ->
        ins $ flip UVec.copy vec
      TextLineBytes (ByteVector vec) ->
        ins $ \ targ ->
        UVec.iforM_ vec $ \ i ->
        UMVec.write targ i . fromChar8
    where
    ins :: (UMVec.IOVector Char -> IO ()) -> EditLine tags ()
    ins copySrc = do
      let txtlen = textLineContentLength txt
      editLineLiftGB $ do
        GapBuf.ensureFreeSpaceWith (+ txtlen) $ \ evt -> do
          newvec <- use gapBufferVector
          let toNew copyMove start src =
                copyMove (UMVec.slice start (UMVec.length src) newvec) src
          (~lo, _gap, ~hi) <- gapBuffer3Slice
          (>>= (liftIO . copySrc)) $ case dir of
            Before -> do
              liftIO $ case evt of
                Nothing  -> toNew UMVec.move txtlen lo
                Just evt -> do
                  let old = GapBuf.theOldLowerVector evt
                  toNew UMVec.copy txtlen old
                  UMVec.copy hi $ GapBuf.theOldUpperVector evt
              GapBuf.beforeCursor += txtlen
              pure $ UMVec.slice 0 txtlen newvec
            After  -> do
              after <- use GapBuf.afterCursor
              let newlen = UMVec.length newvec
              let histart = newlen - after
              liftIO $ case evt of
                Nothing  -> toNew UMVec.move (histart - txtlen) hi
                Just evt -> do
                  let old = GapBuf.theOldUpperVector evt
                  UMVec.copy lo $ GapBuf.theOldLowerVector evt
                  toNew UMVec.copy (histart - txtlen) old
              GapBuf.afterCursor += txtlen
              pure $ UMVec.slice (newlen - txtlen) txtlen newvec
      edlnState $ editLineNonAscii dir += textLineUTFWeight txt

  -- insertLineHere
  --   :: (Monad (editor tags), LineEditor editor)
  --   => RelativeDirection -> TextLine tags -> editor tags ()
  insertLineHere dir line =
    case theTextLineBreak line of
      NoLineBreak -> insertSized
      _ ->
        cutLine After >>=
        pushLine After >>
        case dir of
          After  -> insertLineAtEnd After line
          Before -> insertSized
    where
    lineData = theTextLineData line
    insertSized =
      case lineData of
        TextLineEmpty -> pure ()
        _ -> editLineLiftGB $ do
          let len = stringLength lineData
          GapBuf.ensureFreeSpace (+ len)
          (_ , gap , _) <- gapBuffer3Slice
          let targ = case dir of
                Before -> UMVec.slice 0 len gap
                After  -> UMVec.slice (UMVec.length gap - len) len gap
          liftIO $ case lineData of
            TextLineEmpty -> pure ()
            TextLineChars _ vec -> UVec.copy targ vec
            TextLineBytes (ByteVector vec) ->
              UVec.iforM_ vec $ \ i ->
              UMVec.write targ i . fromChar8

  -- pushChar :: RelativeDirection -> Char -> EditLine tags ()
  pushChar dir = \ case
    '\n' -> newline dir
    c    -> do
      unless (isAscii c) $ edlnState $ editLineNonAscii dir += 1
      editLineLiftGB $ GapBuf.pushItem dir c

  -- popChar :: RelativeDirection -> EditLine tags Char
  popChar dir =
    editLineLiftGB (use GapBuf.beforeCursor) >>= \ count ->
    if count <= 0 then joinLine dir >> popChar dir else
    editLineLiftGB (GapBuf.popItem dir)

  -- tryPopChar :: RelativeDirection -> EditLine tags (Maybe Char)
  tryPopChar dir =
    let (EditLine f) = popChar dir in
    EditLine $ 
    f >>= \ case
      EditLinePop{} -> pure $ EditLineOK Nothing
      a              -> pure $ Just <$> a

  -- countChars :: RelativeDirection -> EditLine tags VectorSize
  countChars = editLineLiftGB . use . GapBuf.relativeCursor

  -- cutLine :: RelativeDirection -> EditLine tags (TextLine tags)
  cutLine dir = copyLine dir <* editLineLiftGB (GapBuf.relativeCursor dir .= 0)

  -- copyLine :: RelativeDirection -> EditLine tags (TextLine tags)
  copyLine dir =
    (\ (lo, _gap, hi) ->
      case dir of
        Before -> lo
        After  -> hi
    ) <$>
    editLineLiftGB
    ( gapBuffer3Slice <*
      (GapBuf.relativeCursor dir .= 0)
    ) >>= \ src ->
    edlnState
    ( (,) <$>
      use editLineBreakSymbol <*>
      use (editLineNonAscii dir) <*
      (editLineNonAscii dir .= 0)
    ) >>= \ (lbrk, nonAsciiCount) ->
    liftIO $
    if nonAsciiCount > 0 then
      textLineFromVector (TextLineChars nonAsciiCount) lbrk <$>
      UVec.unsafeFreeze src
    else
      textLineFromVector textLineBytes lbrk <$> do
        targ <- UMVec.new $ UMVec.length src
        UMVec.imapM_ (\ i -> UMVec.write targ i . toChar8Lossy) src
        UVec.unsafeFreeze targ

  -- copyBuffer :: EditLine tags (TextLine tags)
  copyBuffer = do
    w <- editLineUTFWeight
    lbrk <- edlnState $ use editLineBreakSymbol
    editLineLiftGB $
      if w > 0 then
        textLineFromVector (TextLineChars w) lbrk <$> GapBuf.fuseBuffer
      else do
        size <- GapBuf.cursorElemCount
        new <- liftIO $ UMVec.new size
        (lo, _gap, hi) <- gapBuffer3Slice
        let step i c = UMVec.write new i (fromIntegral $ ord c) >> (pure $! i + 1)
        liftIO $
          textLineFromVector textLineBytes lbrk <$>
          ( UMVec.foldM' step 0 lo >>=
            flip (UMVec.foldM' step) hi >>
            UVec.unsafeFreeze new
          )

  -- clearBuffer :: Monad (editor tags) => editor tags ()
  clearBuffer = editLineLiftGB $ do
    GapBuf.beforeCursor .= 0
    GapBuf.afterCursor .= 0

  -- shiftCursor :: RelativeIndex -> EditLine tags RelativeIndex
  shiftCursor r =
    case compare r 0 of
      EQ -> return 0
      LT -> loop negate Before After 0
      GT -> loop id After Before 0
    where
    -- Ordinarily it might be faster to use  the shiftCursor function from the GapBuffer module, but
    -- since  we  need  to keep  track  of  the  number  of  non-ASCII  characters, it  is  no  more
    -- computationally difficult to pull, inspect, and push each character one at a time.
    stop = abs r
    loop neg dir other count =
      if count >= stop then pure $ neg count else
      editLineLiftGB (use $ GapBuf.relativeCursor dir) >>= \ remaining ->
      if remaining <= 0 then pure $ neg count else
      editLineLiftGB (GapBuf.pullItem dir) >>= \ c ->
      if c == '\n' then pure $ neg count else do
        editLineLiftGB $ do
          GapBuf.relativeCursor dir -= 1
          GapBuf.pushItem other c
        unless (isAscii c) $ edlnState $ do
          editLineNonAscii dir -= 1
          editLineNonAscii other += 1
        loop neg dir other $! count + 1

----------------------------------------------------------------------------------------------------

-- | Access the internals of the 'EditLineState'.  The 'EditLine' function type does not instantiate
-- the 'MonadState' typeclass because the state internal to the 'EditLine' function context contains
-- values such as 'editLineBreakSymbol' that can be set by code using this function type, but cannot
-- be  set to  an  arbitrary value  ('NoLineBreak' is  disallowed).  Instantiating the  'MonadState'
-- typeclass would allow methods such as 'modify' or 'put' to set arbitrary values.
--
-- In order to allow  more limited access to the internal state of  the 'EditLine' function context,
-- this gate-keeping function is exposed as part of  the API. Currently this function is only useful
-- with the 'editLineTokenizer' and 'editLineBreakSymbol' 'Lens'-es, which allows you to retrieve or
-- set the 'TokenizerTable' used for line break parsing.
onEditLineState :: State (EditLineState tags) a -> EditLine tags a
onEditLineState f = edlnState $
  use editLineBreakSymbol >>= \ lbrk ->
  f <*
  ( editLineBreakSymbol %= \ case
      NoLineBreak -> lbrk
      lbrk -> lbrk
  )

-- | Not for export. This is the fast but unsafe function for changing the 'EditLineState'.
edlnState :: State (EditLineState tags) a -> EditLine tags a
edlnState = EditLine . fmap EditLineOK . state . runState

-- | This is the mutable text that can be edited.
editLineGapBuffer :: Lens' (EditLineState tags) (GapBufferState UMVec.MVector Char)
editLineGapBuffer = lens theEditLineGapBuffer $ \ a b -> a{ theEditLineGapBuffer = b }

-- | Keeps track of the number of non-ASCII characters before the cursor.
editLineNonAsciiBefore :: Lens' (EditLineState tags) Int
editLineNonAsciiBefore =
  lens theEditLineNonAsciiBefore $ \ a b -> a{ theEditLineNonAsciiBefore = b }

-- | Keeps track of the number of non-ASCII characters after the cursor.
editLineNonAsciiAfter :: Lens' (EditLineState tags) Int
editLineNonAsciiAfter =
  lens theEditLineNonAsciiAfter $ \ a b -> a{ theEditLineNonAsciiAfter = b }

editLineNonAscii :: RelativeDirection -> Lens' (EditLineState tags) Int
editLineNonAscii = \ case
  Before -> editLineNonAsciiBefore
  After  -> editLineNonAsciiAfter

-- | Set the default line break symbol. This symbol will be used when evaluating 'newline'.
editLineBreakSymbol :: Lens' (EditLineState tags) LineBreakSymbol
editLineBreakSymbol = lens theEditLineBreakSymbol $ \ a b -> a{ theEditLineBreakSymbol = b }

-- |  Change the  'TokenizerTable'  used for  parsing  line breaks.  This table  will  be used  when
-- evaluating 'streamFoldLines', 'hFoldLines', and 'byteStreamDecode'.
editLineTokenizer :: Lens' (EditLineState tags) (TokenizerTable LineBreakSymbol LineBreakerState)
editLineTokenizer = lens theEditLineTokenizer $ \ a b -> a{ theEditLineTokenizer = b }

-- |  Initialize a  new 'EditLineState'.  You may  initialize  it with  a 'TextLine',  which may  be
-- 'emptyTextLine' to fill the 'EditLine' buffer.
--
-- You can  also specify how to  position the cursor  with 'Either' a 'VectorIndex',  specifying how
-- many positions from the 'Left' (beginning) of the  buffer to place the cursor, or you can specify
-- that   you  want   the   cursor  @'Right'   'Before'@  or   @'Right'   'After'@  the   characters
-- inserted. (Appologies to people who prefer right-to-left written languages, or for those who feel
-- that the right is the beginning of the buffer.)
newEditLineState
  :: CharBufferSize
  -> IO (EditLineState tags)
newEditLineState initSize =
  newEditorState >>=
  runEditor (newCurrentBuffer $ maybe 512 id initSize) >>= \ ((), buf) ->
  return EditLineState
  { theEditLineGapBuffer      = GapBuf.editorToGapBufferState buf
  , theEditLineNonAsciiBefore = 0
  , theEditLineNonAsciiAfter  = 0
  , theEditLineBreakSymbol    = LineBreakLF
  , theEditLineTokenizer      = lineBreak_LF
  }

-- |  Run a  single  iteration of  an  'EditLine' function.  Iteration  halts in  the  event that  a
-- 'pushLine' or  'popLine' is evaluated. Inspect  the 'EditLineResult' to extract  the continuation
-- function and  decide which action  to take  for either event.  Use 'streamEditor' instead  if you
-- would like to automatically handle these events by pushing 'TextLine's onto a stack.
runEditLine
  :: EditLine tags a
  -> EditLineState tags
  -> IO (EditLineResult EditLine tags a, EditLineState tags)
runEditLine (EditLine f) = runStateT f

-- | Evaluate  an 'EditLine' function,  but capture it's  'EditLineResult', even errors  and halting
-- conditions, and return the captured 'EditLineResult'.
catchEditLineResult :: EditLine tags a -> EditLine tags (EditLineResult EditLine tags a)
catchEditLineResult (EditLine f) = EditLine $ EditLineOK <$> f

-- | Get the current 'CharIndex' for the cursor in an 'EditText' function editing a line of text.
columnNumber :: EditLine tags CharIndex
columnNumber =
  toIndex . GaplessIndex . subtract 1 <$>
  editLineLiftGB (use GapBuf.beforeCursor)

-- | Lift a 'GapBuffer' function into an 'EditLine' function context.
editLineLiftGB :: GapBuffer UMVec.MVector Char a -> EditLine tags a
editLineLiftGB f = do
  (result, gapbuf) <- edlnState (use editLineGapBuffer) >>= liftIO . runGapBuffer f
  edlnState $ editLineGapBuffer .= gapbuf
  case result of
    Left err -> throwError $ EditLineError err
    Right ok -> pure ok

minCharIndex :: EditLine tags CharIndex
minCharIndex = pure 1

-- | Return the maximum 'CharIndex' value for the current 'EditLineState' of the 'EditLine' function
-- context. An empty 'EditLineState' will return a value of 0.
maxCharIndex :: EditLine tags CharIndex
maxCharIndex =
  toIndex . GaplessIndex . subtract 1 <$>
  editLineLiftGB GapBuf.cursorElemCount

-- | An accounting of the number of non ASCII characters ('Char' values not satisfying 'isAscii') in
-- the current 'EditLineState'.
editLineUTFWeight :: EditLine tags Int
editLineUTFWeight =
  edlnState $
  (+) <$> use editLineNonAsciiBefore <*> use editLineNonAsciiAfter

textLineFromVector
  :: UMVec.Unbox c
  => (UVector c -> TextLineData)
  -> LineBreakSymbol
  -> UVector c
  -> TextLine tags
textLineFromVector constr lbrk vec =
  emptyTextLine
  { theTextLineData  = constr vec
  , theTextLineBreak = lbrk
  }

-- | Evaluates 'cutLine'  to construct a 'TextLine'  and clear the current  'EditLine' buffer.  Then
-- evaluates 'pushLine' with the constructed 'TextLine'.
newline
  :: (Monad (editor tags), LineEditor editor)
  => RelativeDirection -> editor tags ()
newline dir = cutLine dir >>= pushLine dir

-- |  Like  'newline'  but  sets  'theTextLineBreak'  of  the  resultant  'TextLine'  to  the  given
-- 'LineBreakSymbol'.  'NoLineBreak'  values are  ignored,  so  if  the given  'LineBreakSymbol'  is
-- 'NoLineBreak' this function behaves exactly the same as 'newline'.
lineBreak
  :: (Monad (editor tags),  LineEditor editor)
  => RelativeDirection -> LineBreakSymbol -> editor tags ()
lineBreak dir lbrk =
  (\ txt -> case lbrk of
      NoLineBreak -> txt
      _ -> txt{ theTextLineBreak = lbrk }
  ) <$>
  cutLine dir >>=
  pushLine dir

-- | Calls  'pushChar' for  each item  in the  given 'String'.  This function  is simply  defined as
-- @'mapM_' ('pushChar' 'Before')@, it  is only here becuase it is expected to  be used often enough
-- that having a separate function for this feature might be convenient. This function does not take
-- a  'RelativeDirection', in  must push  the  given 'String'  in  the 'Before'  direction to  avoid
-- computing the length of the string, and to avoid parsing line breaking characters.
insertString :: (Monad (editor tags), LineEditor editor) => String -> editor tags ()
insertString = mapM_ $ pushChar Before

-- |  Insert any  'FoldableString' into  the text  buffer. Like  'pushChar' and  'insertString', the
-- @'\\n'@ character evaluates 'newline'. This function does not take a 'RelativeDirection', in must
-- push the given  @string@ in the 'Before' direction  to avoid computing the length  of the string,
-- and to avoid parsing line breaking characters.
insert
  :: (FoldableString string, Monad (editor tags), LineEditor editor)
  => string -> editor tags ()
insert = foldString (\ f c -> f >> pushChar Before c) (pure ())

-- | Ask the evaluation context for a 'TextLine' from 'Before' (above) or 'After' (below) the
-- current line, it will be inserted into the current 'EditLine' excluding the line breaking
-- character.
joinLine
  :: (Monad (editor tags), LineEditor editor)
  => RelativeDirection -> editor tags ()
joinLine dir = popLine dir >>= insertLineAtEnd dir

-- | Copy the  buffer content and clear it.  This is a convenience function  that calls 'copyBuffer'
-- followed by 'clearBuffer', and  returns the result from 'copyBuffer' while  also setting the line
-- break symbol of the 'TextLine'. Neither the 'lineBreak' function (nor the 'newline' function) are
-- evaluated at  all, so  evaluating this  function as part  of a  'TextEdit' computation  pushes no
-- information to the text buffer.
--
-- You may  set any 'LineBreakSymbol'  at all, including 'NoLineBreak',  since this function  has no
-- effect on anything outside of the 'LineEditor' context.
copyBufferClear
  :: (Monad (editor tags), LineEditor editor)
  => LineBreakSymbol
  -> editor tags (TextLine tags)
copyBufferClear lbrk =
  (textLineBreakSymbol .~ lbrk) <$>
  (copyBuffer <* clearBuffer)

-- | This function  is the easiest way to  evaluate an 'EditLine' function without the  backing of a
-- 'TextBuffer' to keep  lines pushed or poped by  'pushLine' or 'popLine'. It uses a  stack to keep
-- lines pushed by 'pushLine' in both the  'Before' and 'After' directions, throwing an exception if
-- 'popLine'  is evaluated  on an  empty stack  (a 'GapBufferError'  is thrown,  though there  is no
-- 'GapBuffer' involved). If the  initial 'EditLine' function given is evaluates  a function such as
-- 'hFoldLines', this function behaves somewhat similar to the UNIX @sed@ program.
--
-- __NOTE:__ it may seem as though this function belongs in the "VecEdit.Text.Stream" module, but it
-- is  a  specific  implementation   of  the  'EditLine'  function,  and  so   is  defined  in  this
-- "VecEdit.Text.Line.Editor" module instead.
streamEditor
  :: EditLine tags a
  -> EditLineState tags
  -> IO (Either EditTextError a, EditLineState tags)
streamEditor = loop [] [] where
  err = Left . EditorPrimOpError . PopItem
  loop before after f st =
    runEditLine f st >>= \ (result, st) ->
    case result of
      EditLineOK a -> pure (Right a, st)
      EditLineFail err -> pure (Left err, st)
      EditLinePush dir line next ->
        case dir of
          Before -> loop (line:before) after next st
          After  -> loop before (line:after) next st
      EditLinePop dir next ->
        case dir of
          Before ->
            case before of
              [] -> pure (err dir, st)
              line:before -> loop before after (next line) st
          After ->
            case after of
              [] -> pure (err dir, st)
              line:after -> loop before after (next line) st

----------------------------------------------------------------------------------------------------

-- | A class of stateful data types that can produce a stream 'Char8' byte values.
class IOByteStream stream where
  streamEnded :: stream -> IO Bool
  streamNext :: stream -> IO Char8

instance IOByteStream Handle where
  streamEnded = hIsEOF
  streamNext = fmap toChar8Lossy . hGetChar 

instance IOByteStream (IORef ByteString) where
  streamEnded = fmap Bytes.null . readIORef
  streamNext ioref = do
    bytes <- readIORef ioref
    writeIORef ioref $ Bytes.drop 1 bytes
    return $ Bytes.head bytes

instance IOByteStream (IORef LazyBytes.ByteString) where
  streamEnded = fmap LazyBytes.null . readIORef
  streamNext ioref = do
    bytes <- readIORef ioref
    writeIORef ioref $ LazyBytes.drop 1 bytes
    return $ LazyBytes.head bytes

-- | Treats an 'EditorState' as a byte stream. The stream of characters is limited to the
-- 'currentRange'.
newtype EditorStream = EditorStream (IORef (EditorState UMVec.MVector Char8))

instance IOByteStream EditorStream where
  streamEnded (EditorStream ioref) = do
    st <- readIORef ioref
    let cur = st ^. currentCursor
    let len = st ^. currentRange ^. rangeLength
    pure (cur >= len)
  {-# INLINE streamEnded #-}
  streamNext (EditorStream ioref) = do
    st <- readIORef ioref
    let cur = st ^. currentCursor
    let off = st ^. currentRange ^. rangeStart
    writeIORef ioref $ currentCursor +~ 1 $ st
    UMVec.read (st ^. currentBuffer) (off + cur)
  {-# INLINE streamNext #-}

-- | Construct an 'EditorStream' which can be used as an argument to 'streamFoldLines'.
newEditorStream :: EditorState UMVec.MVector Char8 -> IO EditorStream
newEditorStream st = EditorStream <$> newIORef (currentCursor .~ 0 $ st)

-- | Construct an 'IORef' containing a strict 'ByteStream', so that the 'IORef' can be used as an
-- argument to 'streamFoldLines'.
streamByteString :: ByteString -> IO (IORef ByteString)
streamByteString = newIORef

-- | Construct  an 'IORef' containing  a lazy 'ByteStream',  so that the 'IORef'  can be used  as an
-- argument to 'streamFoldLines'.
streamLazyByteString :: LazyBytes.ByteString -> IO (IORef LazyBytes.ByteString)
streamLazyByteString = newIORef

----------------------------------------------------------------------------------------------------

-- | This is the state  for a UTF-8 decoder function. You feed bytes in,  it produces chars or error
-- states.
data UTF8Decoder
  = UTF8Decoder
    { theUTF8Countdown :: !Word16
    , theUTF8ElemCount :: !Word16
    , theUTF8ElemStore :: !Word32
    , theUTF8DecodedValue :: !Word32
    , theUTF8ByteCounter :: !Int
    , theUTF8CharCounter :: !Int
    }

-- | This function is evaluated when a byte is received but no char is emitted.
type UTF8DecodeStep a = State UTF8Decoder a

-- | This function is evaluated when a 'Char' is successfully decoded.
type UTF8DecodeTakeChar a = Char -> State UTF8Decoder a

-- | This function is evaluated when a bad character is found. It takes 2 arguments:
--
--  1. the number of bytes decoded thus far
--
--  2. the   bytes   themselves,  merged   together  into   a  'Word32'   with   earlier   bytes  in
--     higher-significant  positions. Use  'utf8ErrorDecompose'  to  break this  value  up into  the
--     sequence of bytes that caused the error.
--
type UTF8DecodeTakeError a = State UTF8Decoder a

-- | An initialized 'UTF8Decoder'
utf8Decoder :: UTF8Decoder
utf8Decoder = UTF8Decoder 0 0 0 0 0 0

-- | When a multi-byte character is encountered, this counts the number of remaining component bytes
-- expected in the stream.
utf8Countdown :: Lens' UTF8Decoder Word16
utf8Countdown = lens theUTF8Countdown $ \ a b -> a{ theUTF8Countdown = b }

-- | Number of elements in the store
utf8ElemCount :: Lens' UTF8Decoder Word16
utf8ElemCount = lens theUTF8ElemCount $ \ a b -> a{ theUTF8ElemCount = b }

-- | Stores the sequence of bytes decoded so far, used for debugging.
utf8ElemStore :: Lens' UTF8Decoder Word32
utf8ElemStore = lens theUTF8ElemStore $ \ a b -> a{ theUTF8ElemStore = b }

-- | The actual decoded value
utf8DecodedValue :: Lens' UTF8Decoder Word32
utf8DecodedValue = lens theUTF8DecodedValue $ \ a b -> a{ theUTF8DecodedValue = b }

-- | The total number of bytes inspected by the decoder.
utf8ByteCounter :: Lens' UTF8Decoder Int
utf8ByteCounter = lens theUTF8ByteCounter $ \ a b -> a{ theUTF8ByteCounter = b }

-- | The total number of characters emitted by the decoder.
utf8CharCounter :: Lens' UTF8Decoder Int
utf8CharCounter = lens theUTF8CharCounter $ \ a b -> a{ theUTF8CharCounter = b }

-- | This  function constructs  a 'UTF8DecodeTakeError'  function that  calls the  given contination
-- anywhere from 1 to 4 times, each time giving  it a 'Char8' from the invalid UTF-8 character found
-- in the stream.
utf8ErrorDecompose :: (fold -> Char8 -> State UTF8Decoder fold) -> fold -> UTF8DecodeTakeError fold
utf8ErrorDecompose f fold = do
  bytes <- use utf8ElemStore
  count <- fromIntegral <$> use utf8ElemCount
  let loop count fold = 
        if count <= 0 then pure fold else
        f fold (fromIntegral $ 0x000000FF .&. (bytes `shift` (-8) * count)) >>=
        (loop $! count - 1)
  loop count fold

-- | This function steps the 'UTF8Decoder' state  with a single byte. Evaluating 'runState' yields a
-- pure  function, so  it  can be  run  anywhere with  little  overhead. This  function  is used  by
-- 'streamFoldLines' and 'hFoldLines', which in turn passes  it as the byte handling continuation in
-- the 'byteStreamDecode' function.
utf8DecoderPushByte
  :: Bool
  -> UTF8DecodeStep a
  -> UTF8DecodeTakeChar a
  -> UTF8DecodeTakeError a
  -> Char8
  -> State UTF8Decoder a
utf8DecoderPushByte doStore step emit0 fail0 w =
  use utf8Countdown >>= \ case
    0 -> case w of
      w | w .&. 0x80 == 0x00 ->
        (utf8DecodedValue .= up w) >>
        (byte1 >> elem1 >> char1 >> decode >>= emit0) <*
        reset
      w | w .&. 0xE0 == 0xC0 -> expect 1 0xE0
      w | w .&. 0xF0 == 0xE0 -> expect 2 0xF0
      w | w .&. 0xF8 == 0xF0 -> expect 3 0xF8
      _ -> fail
    1 -> pushBits emit
    2 -> pushBits countdown
    3 -> pushBits countdown
    i -> error $ "internal error: utf8DecoderPushByte invalid state " <> show i
  where
    up = fromIntegral :: Char8 -> Word32
    char1 = utf8CharCounter += 1
    byte1 = utf8ByteCounter += 1
    elem1 = utf8ElemCount += 1
    reset = do
      utf8Countdown .= 0
      when doStore $ do
        utf8ElemStore .= 0
        utf8ElemCount .= 0
    store =
      when doStore $
      byte1 >> elem1 >>
      (utf8ElemStore %= \ sto -> shift sto 8 .|. up w)
    decode = chr . fromIntegral <$> use utf8DecodedValue
    emit =
      store >> char1 >> decode >>= \ c ->
      -- Check if an ASCII character was encoded with multiple bytes. This is not allowed.
      if isAscii c then fail else
      emit0 c <* reset
    fail = store >> fail0 <* reset
    countdown = utf8Countdown -= 1 >> step
    expect i invMask = do
      utf8ElemStore .= 0
      utf8Countdown .= i
      store
      utf8DecodedValue .= (complement invMask .&. up w)
      step
    pushBits next =
      store >>
      if w .&. 0xC0 /= 0x80 then fail else
      (utf8DecodedValue %= (.|. (up (w .&. 0x3F))) . flip shift 6) >>
      next

{-# INLINE utf8DecoderPushByte #-}

----------------------------------------------------------------------------------------------------

-- |  This function  type is  used almost  exclusively with  the 'hFoldLines'  and 'streamFoldLines'
-- functions, which are functions  for performing some transformation (a fold) on  the content of an
-- 'IOByteStream'. These functions will read through an  'IOByteStream' until a line break symbol is
-- found. Upon finding  a line break, a  continuation (or "callback") of  this 'EditStream' function
-- type is evaluated,  allowing the 'EditLineState' (which contains the  mutable 'LineBuffer') to be
-- modified,  and 'TextLine's  to be  generated. When  evaluating 'hFoldLines'  or 'streamFoldLines'
-- within  an 'VecEdito.Text.Editor.EditText'  function context,  modifications  can be  made to  an
-- 'IOByteStream' as it is buffered into a 'VecEdito.Text.Editor.TextBuffer'.
--
-- This 'EditStream' function type instantiates 'LineEditor' and so can edit lines of text using the
-- same APIs as the 'EditLine' function type. But it also instantiates 'MonadCont', which means that
-- functions such  as 'hFoldLines' and 'streamFoldLines',  both of which take  continuations of type
-- 'EditStream',  also pass  a halting  function of  type 'HaltEditStream'  to these  continuations,
-- allowing the fold loop to be halted if some condition is met.
--
-- Those familiar with the UNIX/Linux operating system may know about the @sed@ utility program used
-- in the CLI, and that the name @sed@ is a contraction of "__S__tream __ED__itor." And indeed, this
-- 'EditStream'  function  type  has similar  functionality,  though  you  may  edit with  the  full
-- 'EditLine' API  rather than  merely using  regular expressions  with capture  groups to  find and
-- replace text patterns.
newtype EditStream fold tags a
  = EditStream
    { unwrapEditStream ::
        ContT
        (EditLineResult (EditStream fold) tags (), EditStreamState tags fold)
        (StateT (EditStreamState tags fold) IO)
        (EditLineResult (EditStream fold) tags a)
    }

instance MonadCont (EditStream fold tags) where
  callCC f =
    EditStream $
    callCC $ \ halt ->
    unwrapEditStream $ f $
    EditStream <$> (halt . EditLineOK)

-- | Functions  of this  type are  constructed by stream  reader functions  such as  'hFoldLines' or
-- 'streamFoldLines' evaluating 'callCC'  prior to entering the stream reading  loop, and are passed
-- to the loop continuation functions of type  'EditStream' so that these continuation functions may
-- evaluate the 'HaltEditStream' function to halt the stream reading loop.
--
-- This function "returns" a 'Void' data type so when  you use it, you may need to use the 'vacious'
-- function like so:
--
-- @
--  (\\ halt line ->
--    do 'when' ('textLineBreakSymbol' == 'NoLineBreak') $
--           'vacuous' $ 'get' >>= halt 
--       ...
--  )
-- @
type HaltEditStream fold tags = fold -> EditStream fold tags Void

-- | This is the stateful context of the 'EditStream' function type.
--
-- __Note:__ the @tags@ and @fold@ variables are reversed from the 'EditStream' monad. This makes it
-- easier  to instantiate  'EditStreamState'  into  the 'Functor'  typeclass  and 'EditStream'  into
-- 'LineEditor' typeclass.
data EditStreamState tags fold
  = EditStreamState
    { theEditStreamState       :: !fold
    , theEditStreamUTF8Decoder :: !UTF8Decoder
    , theEditStreamLineEditor  :: !(EditLineState tags)
    , theEditStreamHandleUTFDecoderError :: !(HandleUTFDecodeError fold tags)
    }

-- |  This  is  the  type  of  function  used by  'byteStreamDecode'  to  respond  to  UTF  decoding
-- errors. Passing 'Nothing' tells the decoder to silently drop the bad bytes.
type HandleUTFDecodeError fold tags =
  Maybe (HaltEditStream fold tags -> UTF8Decoder -> EditStream fold tags ())

instance Functor (EditStream fold tags) where
  fmap f (EditStream ma) = EditStream $ fmap f <$> ma

instance Applicative (EditStream fold tags) where
  pure = return
  (<*>) = ap

instance Monad (EditStream fold tags) where
  return = EditStream . return . EditLineOK
  (EditStream f) >>= ma =
    EditStream $
    f >>= \ case
      EditLineOK a -> unwrapEditStream $ ma a
      EditLineFail err -> pure $ EditLineFail err
      EditLinePush dir line next -> pure $
        EditLinePush dir line $ next >>= ma
      EditLinePop  dir      next -> pure $
        EditLinePop  dir      $ next >=> ma

instance MonadIO (EditStream fold tags) where
  liftIO = EditStream . liftIO . fmap EditLineOK

instance MonadState fold (EditStream fold tags) where
  state f =
    EditStream $ lift $ state $ \ st ->
    let (a, fold) = f (theEditStreamState st) in
    (EditLineOK a, st{ theEditStreamState = fold })

instance MonadError EditTextError (EditStream fold tags) where
  throwError = EditStream . pure . EditLineFail
  catchError (EditStream try) catch =
    EditStream $
    try >>= \ case
      EditLineFail err -> unwrapEditStream $ catch err
      result -> pure result

instance MonadFail (EditStream fold tags) where
  fail = throwError . EditTextFailed . Strict.pack

instance LineEditor (EditStream fold) where
  editLineLiftResult = EditStream . pure
  liftEditLine f =
    onEditStreamState get >>= \ readst ->
    liftIO (runEditLine f $ readst ^. editStreamLineEditor) >>= \ (result, edst) ->
    onEditStreamState (editStreamLineEditor .= edst) >>
    case result of
      EditLineOK a -> pure a
      EditLineFail err -> throwError err
      EditLinePush dir line next ->
        editLineLiftResult $
        EditLinePush dir line $
        liftEditLine next
      EditLinePop dir next ->
        editLineLiftResult $
        EditLinePop dir $
        liftEditLine . next

-- | The value over which the 'EditStream' function type is folding. Use this lens to view or update
-- the @fold@ value within a 'EditStreamState' value.
editStreamState :: Lens' (EditStreamState tags fold) fold
editStreamState = lens theEditStreamState $ \ a b -> a{ theEditStreamState = b }

-- | The 'UTF8Decoder' state.
editStreamUTF8Decoder :: Lens' (EditStreamState tags fold) UTF8Decoder
editStreamUTF8Decoder = lens theEditStreamUTF8Decoder $ \ a b -> a{ theEditStreamUTF8Decoder = b }

-- | The 'EditLineState' used to lift 'EditLine' functions into 'EditStream' functions.
editStreamLineEditor :: Lens' (EditStreamState tags fold) (EditLineState tags)
editStreamLineEditor = lens theEditStreamLineEditor $ \ a b -> a{ theEditStreamLineEditor = b }

editStreamHandleUTFDecoderError :: Lens' (EditStreamState tags fold) (HandleUTFDecodeError fold tags)
editStreamHandleUTFDecoderError =
  lens theEditStreamHandleUTFDecoderError $ \ a b ->
  a{ theEditStreamHandleUTFDecoderError = b }

-- | Default buffer size for the 'hFoldLines' function.
hEditStreamBufferSize :: Int
hEditStreamBufferSize = 65536

-- | Initialize a new 'EditStreamState' so that you can evalaute functions such as 'streamFoldLines'
-- or 'hFoldLines'. The 'EditStreamState' contains a 'UTF8Decoder' which might be in an inconsistent
-- state when 'streamFoldLines'  ends, since a 'EditStream' function may  terminate execution before
-- reaching the end of a  stream, and while in the middle of decoding a  UTF-8 character. It is best
-- to keep the 'EditStreamState' value returned  by 'streamFoldLines' or 'hFoldLines' for evaluation
-- on  the  remainder  of  the  stream,  should  the  'EditStream'  function  terminate  under  such
-- circumstances.
newEditStreamState :: fold -> EditLine tags (EditStreamState tags fold)
newEditStreamState fold =
  onEditLineState get >>= \ edst ->
  pure $
  EditStreamState
  { theEditStreamState       = fold
  , theEditStreamLineEditor  = edst
  , theEditStreamUTF8Decoder = utf8Decoder
  , theEditStreamHandleUTFDecoderError = Nothing
  }

onEditStreamState :: State (EditStreamState tags fold) a -> EditStream fold tags a
onEditStreamState = EditStream . lift . state . runState . fmap EditLineOK

-- | Not for export, it is impossible to call this without a 'EditStreamState', and it is impossible
-- to construct a 'EditStreamState' outside of  the 'EditLine' monad. This function simply evaluates
-- a 'EditStream' function in the IO monad.
runEditStreamIO
  :: EditStream fold tags a
  -> EditStreamState tags fold
  -> IO (EditLineResult (EditStream fold) tags (), EditStreamState tags fold)
runEditStreamIO (EditStream f) readst =
  flip runStateT readst $
  runContT ((,) <$> (fmap (const ()) <$> f) <*> lift get) pure >>= \ (result, readst) ->
  put readst >> pure result

-- |  Evaluate  a  'EditStream'  function  within   an  'EditLine'  function  context,  reusing  the
-- 'LineBuffer' of the current  'EditLine' function context as the same  buffer for the 'EditStream'
-- function.  This function  does  not perform  any  iteration or  folding  unless the  'EditStream'
-- continuation  function given  itself performs  some iteration.   This function  is typically  not
-- useful on it's own, use 'hFoldLines' or 'streamFoldLines' instead.
runEditStream
  :: EditStream fold tags a
  -> EditStreamState tags fold
  -> EditLine tags (Either EditTextError (), EditStreamState tags fold)
runEditStream f readst =
  onEditLineState get >>= \ edst ->
  liftIO (runEditStreamIO f $ editStreamLineEditor .~ edst $ readst) >>= \ (result, readst) ->
  onEditLineState (put $ readst ^. editStreamLineEditor) >>
  case result of
    EditLineOK a -> pure (Right a, readst)
    EditLineFail err -> pure (Left err, readst)
    EditLinePush dir line next ->
      editLineLiftResult $
      EditLinePush dir line $
      runEditStream next readst
    EditLinePop dir next ->
      editLineLiftResult $
      EditLinePop dir $ \ line ->
      runEditStream (next line) readst

-- | This  function reads a  file 'Handle'  similar to how  'hGetContents' works, but  buffering all
-- characters into a 'TextLine'  structure until a line breaking character is  found.  Once the line
-- break  character is  received, or  the end  of the  file 'Handle'  stream is  reached, the  given
-- continuation  is evaluated  which  can then  use  the  'LineEditor' functions  to  edit the  line
-- buffer.  You  can  retrieve  the  whole  content  of  the  'LineBuffer'  as  a  'TextLine'  using
-- 'copyBuffer'.
--
-- The 'Handle'  will be set  to 'char8' encoding  and will have  buffering disabled since  a buffer
-- already exists. UTF-8 decoding errors are ignored and the raw bytes are pushed to the buffer.
--
-- You can  pass a special handler  function of type  'HandleUTFDecodeError' for taking care  of UTF
-- decoder errors, or you can pass 'Nothing' to silently drop bad bytes.
hFoldLines
  :: Handle
  -> (HaltEditStream fold tags -> LineBreakSymbol -> EditStream fold tags ())
  -> EditStreamState tags fold
  -> EditLine tags (Either EditTextError (), EditStreamState tags fold)
hFoldLines handle f fold = do
  liftIO $ hSetByteStreamMode handle
  streamFoldLines handle f fold

-- | This function is  similar to 'hFoldLines', but is more general. Rather  than taking a 'Handle',
-- it takes any 'IOByteStream' type (including 'Handle').  If the @handle@ type variable really is a
-- file 'Handle'  from the  "System.IO" module, use  'hFoldLines' instead, or  at least  ensure that
-- 'hSetByteStreamMode' is  called on the  @handle@ prior to evaluating  this function, you  you may
-- introduce character encodings bugs. Sorry, this is just one of those bugs the Haskell type system
-- cannot help you avoid.
streamFoldLines
  :: IOByteStream handle
  => handle
  -> (HaltEditStream fold tags -> LineBreakSymbol -> EditStream fold tags ())
  -> EditStreamState tags fold
  -> EditLine tags (Either EditTextError (), EditStreamState tags fold)
streamFoldLines h f =
  byteStreamDecode h
  (\ halt lbrk -> f halt lbrk >> clearBuffer) >=> \ (result, readst) ->
  onEditLineState (put $ theEditStreamLineEditor readst) >>
  pure (result, readst)

{-# INLINE streamFoldLines #-}

-- | It is important to set a 'Handle' to byte stream mode when reading it with 'hFoldLines' or
-- 'byteStreamDecode'.
hSetByteStreamMode :: Handle -> IO ()
hSetByteStreamMode h = do
  hSetEncoding h char8
  hSetBuffering h NoBuffering

----------------------------------------------------------------------------------------------------

-- | A lighter-weight, more easily optimized version of 'EditStream' for internal use only. The type
-- of the monad  transformer seems complicated, but that  is only because the type  is very specific
-- for solving the a very particular problem.
--
-- The goal is to  make writing to a 'LineBuffer' happen fast within  a 'StreamDecode' function, and
-- then when the  'LineBuffer' is terminated by  a newline, evaluate a  'EditStream' continuation on
-- that 'LineBuffer',  differring error checking  until the continuation  is ready to  be evaluated.
-- The 'StreamDecode' function type can throw errors or  stop on a line break with 'sdEndLine', this
-- is done by using a "halting" function produced  by 'callCC', so program flow can jump directly to
-- the error checking code, rather than checking  the error condition on every single character that
-- is input.
-- 
-- Refer to the documentation of the 'runStreamDecode' function for how this is accomplished.
--
-- To this  end, the  'StreamDecode' monad  tranformer stack  contains a  reader with  the "halting"
-- function produced by  'callCC' as the reader  environment. The 'sdFinal' function can  be used to
-- call this halting function taken from the environment.  The reader wraps a state monad specifc to
-- the  decoder, and  this state  monad wraps  a  vector 'Ved.Editor'  for writing  characters to  a
-- 'LineBuffer'.  There  are   no  checks  performed  on   values  of  type  'Either'   or  of  type
-- 'EditLineResult' in  any of these  monads.  Once a  'StreamDecode' monad completes  execution, it
-- returns a  'StreamIteration' function  that is  then evaluated  which performs  all of  the error
-- checks, and returns the next 'StreamDecoder' to be evaluated.
newtype StreamDecode fold tags a
  = StreamDecode
    ( ReaderT (HaltStreamDecode fold tags)
      ( ContT (Either EditTextError (StreamIteration fold tags))
        ( StateT (StreamDecodeState tags fold)
          (Ved.Editor UMVec.MVector Char)
        )
      )
      a
    )
  deriving
  ( Functor, Applicative, Monad, MonadIO,
    MonadState (StreamDecodeState tags fold),
    MonadReader (HaltStreamDecode fold tags)
  )

newtype StreamIteration fold tags
  = StreamIteration
    ( EditStream fold tags
      ( StreamDecode fold tags (StreamIteration fold tags)
      , StreamDecodeState tags fold
      )
    )

type HaltStreamDecode fold tags
  =  Either EditTextError (StreamIteration fold tags)
  -> StreamDecode fold tags Void

data StreamDecodeState tags fold
  = StreamDecodeState
    { theSDTable :: !(TokenizerTable LineBreakSymbol LineBreakerState)
    , theSDUTF8Decoder :: !UTF8Decoder
    , theSDWCharCount :: !Int
    , theSDHalt :: HaltEditStream fold tags
    , theSDDecodeUTFErrorHandler :: HandleUTFDecodeError fold tags
    }

newStreamDecodeState
  :: TokenizerTable LineBreakSymbol LineBreakerState
  -> HaltEditStream fold tags
  -> EditStream fold tags (StreamDecodeState tags fold)
newStreamDecodeState table halt =
  onEditStreamState $
  StreamDecodeState table <$>
  use editStreamUTF8Decoder <*>
  pure 0 <*>
  pure (vacuous <$> halt) <*>
  use editStreamHandleUTFDecoderError

-- | This  is the function that  evaluates a 'StreamDecode' function  quickly in between calls  to a
-- 'EditStream'  continuation. The  basic  principle  is that  program  control  is braided  between
-- 'StreamDecode'  functions which  evaluate  quickly when  writing to  a  buffer, and  'EditStream'
-- functions which evaluate arbitrary caller-defined  continuation functions and perform checking on
-- error conditions.
--
-- A  'StreamDecoder'   is  evaluated  with   'runStreamDecoder'  within  a   'EditStream'  function
-- context. The  'StreamIteration' function is a  'EditStream' function that typically  contains the
-- continuation  given to  'streamFoldLines', but  modified  by the  decoder to  return yet  another
-- 'StreamDecoder' function (lets call it "next"). This "next" function is the code that will resume
-- the stream decoding loop. So basically, when a 'runStreamDecoder' runs a 'StreamDecoder' function
-- (lets  call  it  the  "current"  'StreamDecoder'   function),  the  current  function  returns  a
-- 'StreamIteration'  containing the  'EditStream'  continuation function  from 'streamFoldLines'  ,
-- 'runStreamDecoder' evaluates  this 'EditStream' continuation  function which produces  the "next"
-- 'StreamDecoder'. Program flow  is like a braid, alternating between  'EditStream' (which is slow)
-- and 'StreamDecoder' (which is fast). I think functional programmers call this a "Moore Machine."
--
-- __Note that this function will loop infinitely,__ so it is necessary to evaluate 'callCC' and set
-- the halting function provided by 'callCC' as 'theSDHalt' parameter in the 'StreamDecodeState'.
runStreamDecode
  :: forall tags fold . StreamDecode fold tags (StreamIteration fold tags)
  -> StreamDecodeState tags fold
  -> EditStream fold tags
     ( StreamDecode fold tags (StreamIteration fold tags)
     , StreamDecodeState tags fold
     )
runStreamDecode (StreamDecode f) streamState = do
  let vectorBufferLens = editStreamLineEditor . editLineGapBuffer . gapBufferEditorState
  vectorBuffer <- onEditStreamState $ use vectorBufferLens
  ((result, streamState), vectorBuffer) <- liftIO $
    flip Ved.runEditor vectorBuffer $
    flip runStateT streamState $
    flip runContT pure $
    callCC $
    runReaderT (Right <$> f) .
    fmap (StreamDecode . ReaderT . const)
  onEditStreamState $ vectorBufferLens .= vectorBuffer
  updateState streamState
  case result of
    Left err -> throwError err
    Right (StreamIteration next) -> do
      (f, streamState) <- next
      updateState streamState
      runStreamDecode f streamState
  where
  updateState streamState =
    onEditStreamState $
    editStreamUTF8Decoder .=
    streamState ^. sdUTF8Decoder

-- | Evaluate the 'StreamDecode' halting function, either due to an exception, or with a successful
-- end-of-line condition.
sdHaltLine :: Either EditTextError (StreamIteration fold tags) -> StreamDecode fold tags void
sdHaltLine result = ask >>= \ halt -> vacuous $ halt result

-- | Call 'sdHaltLine' with the content of a 'StreamIteration', i.e. the next steps you want the
-- 'StreamDecode' to take.
sdEndLine
  :: EditStream fold tags (StreamDecode fold tags (StreamIteration fold tags))
  -> StreamDecode fold tags void
sdEndLine f = do
  -- Get the wide-char count from the 'StreamDecodeState'
  w <- use sdWCharCount
  -- Reset the sdWCharCount value, and the UTF8Decoder.
  sdWCharCount .= 0
  -- Get the rest of the state, it will be returned to the next 'StreamIteration'.
  st <- get
  sdHaltLine $ Right $ StreamIteration $ do
    -- Transfer the wide-char count to the 'EditLineState' before evaluating 'f'.
    liftEditLine $ edlnState $ editLineNonAsciiBefore .= w
    flip (,) st <$> f

-- | Evaluate the 'EditStream' halting function, passing one final 'EditStream' action to perform.
sdEndStream
  :: (HaltEditStream fold tags -> EditStream fold tags ())
  -> StreamDecode fold tags (StreamIteration fold tags)
sdEndStream final =
  (\ halt -> StreamIteration $ final halt >> get >>= vacuous . halt) <$> use sdHalt

sdTable :: Lens' (StreamDecodeState tags fold) (TokenizerTable LineBreakSymbol LineBreakerState)
sdTable = lens theSDTable $ \ a b -> a{ theSDTable = b }

sdHalt :: Lens' (StreamDecodeState tags fold) (HaltEditStream fold tags)
sdHalt = lens theSDHalt $ \ a b -> a{ theSDHalt = b }

sdWCharCount :: Lens' (StreamDecodeState tags fold) Int
sdWCharCount = lens theSDWCharCount $ \ a b -> a{ theSDWCharCount = b }

sdUTF8Decoder :: Lens' (StreamDecodeState tags fold) UTF8Decoder
sdUTF8Decoder = lens theSDUTF8Decoder $ \ a b -> a{ theSDUTF8Decoder = b }

sdDecodeUTFErrorHandler :: Lens' (StreamDecodeState tags fold) (HandleUTFDecodeError fold tags)
sdDecodeUTFErrorHandler =
  lens theSDDecodeUTFErrorHandler $ \ a b -> a{ theSDDecodeUTFErrorHandler = b }

sdLiftVed :: Ved.Editor UMVec.MVector Char a -> StreamDecode fold tags a
sdLiftVed = StreamDecode . lift . lift . lift

-- |  This function  actually  write a  'Char'  to the  'StreamDecode'  buffer. It  is  used as  the
-- @emitHandler@ given  to the  'sdPushChar8' function,  unless we are  pushing a  known non-Unicode
-- character.
sdPushChar :: Char -> StreamDecode fold tags ()
sdPushChar c = do
  sdLiftVed $ do
    Ved.growBufferWithCursor $ \ size i ->
      if i > size then max 512 $ size * 2 else size
    i <- use currentCursor
    Ved.putElemAt i c
    currentCursor += 1
  unless (isAscii c) $ sdWCharCount += 1

-- | This function simply increments the character  and byte counters of the 'UTF8DecoderState'. The
-- given  'Char' value  is ignored.  It is  used  as the  @emitHandler@ given  to the  'sdPushChar8'
-- function when we are pushing a known non-Unicode character.
sdPushBreak :: Char -> StreamDecode fold tags ()
sdPushBreak _ =
  sdUTF8Decoder %=
  (utf8ByteCounter +~ 1) .
  (utf8CharCounter +~ 1)

-- | This function cycles the UTF8 decoder with the given input byte.
sdPushChar8
  :: (Char -> StreamDecode fold tags ())
  -> StreamDecode fold tags (StreamIteration fold tags)
  -> Char8 -> StreamDecode fold tags ()
sdPushChar8 emitHandler continue c8 = do
  utf8st0 <- use sdUTF8Decoder
  let (next, utf8st) =
        -- UTF8 decoding is pure, so we run the pure function and return a non-pure 'StreamDecode'
        -- function 'next', which is evaluated after updating the 'UTF8DecoderState'.
        flip runState utf8st0 $
        utf8DecoderPushByte False
        (pure $ pure ()) -- This is evaluated on a non-emitting decoding step.
        (pure . emitHandler) -- This is evaluated on a character emitting step.
        ( (\ utf8st -> -- This is evaluated on an error.
            use sdHalt >>= \ halt ->
            use sdDecodeUTFErrorHandler >>=
            maybe (pure ())
            (\ handler -> sdEndLine (handler halt utf8st >> pure continue))
          ) <$>
          get
        )
        c8
  sdUTF8Decoder .= utf8st
  next

-- | This is the most general line breaking function. It takes an arbitrary 'IOByteStream' (which is
-- usally a  'Handle'), and  an arbitrary 'TokenizerTable'  for doing the  line splitting  (which is
-- usually 'lineBreak_LF' on  UNIX-like systems, and 'lineBreak_CRLF' on Windows  systems). It could
-- also be set to 'lineBreak_NUL' or 'allLineBreaks'  (to break on @'\FF'@ and @'\VT'@ characters as
-- well), or any 'TokenizerTable' of your choosing. It also requires a continuation function of type
-- 'EditStream' for pushing 'LineBreakSymbol's.
byteStreamDecode
  :: forall handle fold tags
   . IOByteStream handle
  => handle
  -> (HaltEditStream fold tags -> LineBreakSymbol -> EditStream fold tags ())
  -> EditStreamState tags fold
  -> EditLine tags (Either EditTextError (), EditStreamState tags fold)
byteStreamDecode h pushBreak lineBuffer =
  edlnState (use editLineTokenizer) >>= \ table ->
  flip runEditStream lineBuffer $
  callCC $ newStreamDecodeState table >=>
  (>> get) . lineLoop charLoop
  -- This loops  through the bytes  of an 'IOByteStream', buffering  bytes, and then  evaluating the
  -- given 'EditStream' continuation function @f@ when a  line breaking character is found.  If it's
  -- value  is  '\LF'  or  '\CR',  this  might  be  followed  by  an  accompanying  '\CR'  or  '\LF'
  -- (respectively). After  a complete line  break is  parsed, a line  is emitted by  evaluating the
  -- given  consumer  continuation  @f@, and  @f@  decides  what  to  do  with the  content  of  the
  -- 'GapBuffer'. The whole  loop is lifted into  the 'EditStream' monad, and a  halting function is
  -- produced with 'callCC'  which can be triggered by  the @f@ continuation, or can  be halted when
  -- the end-of-stream is reached.
  where
  cutLine
    :: LineBreakSymbol
    -> StreamDecode fold tags (StreamIteration fold tags)
    -> StreamDecode fold tags (StreamIteration fold tags)
  cutLine lbrk loop =
    use sdHalt >>= \ halt ->
    sdEndLine $
    pushBreak halt lbrk >>
    pure loop

  lineLoop
    :: StreamDecode fold tags (StreamIteration fold tags)
    -> StreamDecodeState tags fold
    -> EditStream fold tags ()
  lineLoop step = runStreamDecode step >=> uncurry lineLoop

  checkStreamEnd
    :: LineBreakSymbol
    -> (Char8 -> StreamDecode fold tags (StreamIteration fold tags))
    -> StreamDecode fold tags (StreamIteration fold tags)
  checkStreamEnd lbrk more =
    -- Check for end of stream, halt if at end, otherwise check for line break.
    liftIO (streamEnded h) >>= \ done ->
    if done then sdEndStream $ flip pushBreak lbrk else
    liftIO (streamNext h) >>= more

  charLoop :: StreamDecode fold tags (StreamIteration fold tags)
  charLoop = checkStreamEnd NoLineBreak checkLineBreak

  checkLineBreak :: Char8 -> StreamDecode fold tags (StreamIteration fold tags)
  checkLineBreak c8 =
    --  We must update the  UTF-8 decoder on every single byte in order  to properly account for the
    -- number of bytes  decoded and characters emitted,  and also to catch UTF-8  decoding errors at
    -- the end of lines.
    let byte pusher = sdPushChar8 pusher charLoop in
    -- Check for a line  break character using 'tokTableLookup'. If the byte from  the stream is not
    -- line breaking, emit the byte to 'sdPushChar8'.
    (flip tokTableLookup $! fromChar8 c8) <$> use sdTable >>= \ (lbrk, st) ->
    case st of
      CharLineBreakNext   -> byte sdPushBreak c8 >> next lbrk
      CharLineBreak       -> byte sdPushBreak c8 >> cutLine lbrk charLoop
      CharNotLineBreaking -> byte sdPushChar  c8 >> charLoop

  next lbrk =
    -- Called by 'checkLineBreak' if it found a '\LF'  or '\CR' character. If this function can pull
    -- one byte from the stream to check for an accompanying '\CR' or '\LF' character, then it emits
    -- a 'TextLine'  with the 2-character  line break. If  the byte pulled  is not line  breaking, a
    -- 'TextLine' is emitted with just the 1-character  line break, but then we immediately charLoop
    -- back immediately to 'checkLineBreak' with the character just pulled.
    checkStreamEnd lbrk $ \ c8 ->
    let c = fromChar8 c8 in
    case lbrk of
      LineBreakLF | c == '\CR' -> cutLine LineBreakLFCR charLoop
      LineBreakCR | c == '\LF' -> cutLine LineBreakCRLF charLoop
      _                        -> cutLine lbrk $ checkLineBreak c8
