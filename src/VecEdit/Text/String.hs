module VecEdit.Text.String
  ( StringData(..), ByteVector(..), byteVectorSize,
    ReadLines(..), HaltReadLines, Word8GapBuffer, Word8GapBufferState,
    runReadLines, readLinesLiftGapBuffer, isAsciiOnly,
    hReadLines, streamReadLines, hSetByteStreamMode,
    cutUTF8StringData, cutUTF8TextLine,
    byteStreamToLines, stringFillCharBuffer,
    hReadLineBufferSize,
    FoldableString(..), StringLength(..),
    FromStringData(..), ToStringData(..), tryConvertString, convertString,
    textFromString, bytesFromString, lazyBytesFromString, byteVecFromString, uvecFromString,
    TextLine, theTextLineBreak, theTextLineData, theTextLineTags,
    textLineTags, textLineBreakSymbol, textLineIsUndefined, undefinedTextLine,
    emptyTextLine, fuseIntoTextLine,
    IOByteStream(..), EditorStream,
    newEditorStream, streamByteString, streamLazyByteString,
    module Data.String
  ) where

import VecEdit.Types
  ( VectorIndex, VectorSize, Range(..), rangeStart, rangeLength,
    RelativeDirection(..), EditTextError(..)
  )

import VecEdit.Text.LineBreak
  ( LineBreakSymbol(..), LineBreakerState(..), lineBreakSize, lineBreak_LF
  )
import VecEdit.Text.TokenizerTable (TokenizerTable, tokTableLookup)
import VecEdit.Vector.Editor
  ( EditorState, currentCursor, currentBuffer, currentRange, freezeCurrentRange,
  )
import VecEdit.Vector.Editor.GapBuffer
  ( GapBuffer, GapBufferState,
    runGapBuffer, withNewGapBuffer, pushItem, fuseBuffer, rethrowErrorInfo,
  )

import Control.Arrow ((***), (>>>), (|||))
import Control.Lens (Lens', lens, use, (^.), (+~), (.~), (.=), (%=))
import Control.Monad (when)
import Control.Monad.Cont (MonadCont(..), ContT, callCC, runContT)
import Control.Monad.Except (MonadError(..), ExceptT(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState(..), StateT(..))
import Control.Monad.Trans (lift)

import qualified Codec.Binary.UTF8.Generic as UTF8
import Codec.Binary.UTF8.Generic (UTF8Bytes(..))
import Codec.Binary.UTF8.String (encodeChar)

import Data.Bits ((.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.ByteString.Builder as BuildBytes
import Data.ByteString.Char8 (ByteString)
import Data.Char (chr, ord)
import Data.String (IsString(..))
import qualified Data.String.UTF8 as UTF8String
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as BuildText
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec

import Data.Word (Word8)

import System.IO
  ( Handle, hSetEncoding, char8, hSetBuffering, BufferMode(NoBuffering),
    hGetChar, hIsEOF
  )

----------------------------------------------------------------------------------------------------

type UVector = UVec.Vector

newtype ByteVector = ByteVector { unwrapByteVector :: UVector Word8 }
  deriving (Eq, Ord, Semigroup, Monoid)

byteVectorSize :: ByteVector -> Int
byteVectorSize (ByteVector str) = UVec.length str

instance UTF8Bytes ByteVector Int where
  bsplit i (ByteVector str) = ByteVector *** ByteVector $ UVec.splitAt i str
  bdrop i (ByteVector str) = ByteVector $ UVec.drop i str
  buncons (ByteVector str) =
    if UVec.null str then Nothing else
    Just (str UVec.! 0, ByteVector $ UVec.tail str)
  elemIndex w (ByteVector str) = UVec.findIndex (== w) str
  empty = ByteVector UVec.empty
  null (ByteVector str) = UVec.null str
  pack = ByteVector . UVec.fromList
  tail (ByteVector str) = ByteVector $ UVec.tail str

----------------------------------------------------------------------------------------------------

-- | This data type encompasses all of Haskell's major immutable text/string data types.
--
-- Note that this data type does instantiate the 'Eq' typeclass, but two strings are only equal if
-- they have the exact same encoding. So @'(==)'@ on 'StringData' is __NOT__ the same as testing if
-- two strings are equal, it is testing if two strings are equal and have the same encoding. So the
-- semantics of the 'StringData' data type is an expression of both a string and also how it is
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

-- | Finding the number of 'Char' values encoded into a string-like data structure. Functions
-- instantiating this typeclass which may require scanning the entire string.
class StringLength   t where { stringLength :: t -> Int; }

-- | Data structures that can be converted from a 'StringData'.
class FromStringData t where { fromStringData :: StringData -> Maybe t; }

-- | Data structures that can be converted to a 'StringData'.
class ToStringData   t where { toStringData   :: Maybe t -> StringData; }

-- | Convert from one string type to another. Be warned that this may cause space leaks when using
-- the usual 'String' data type, as all 'String' values are first converted to a @('UVec.Vector'
-- 'Char')@ type first, which consumes the entire 'String' and store it in memory, before
-- re-encoding it as some other string type.
tryConvertString :: (ToStringData a, FromStringData b) => a -> Maybe b
tryConvertString = fromStringData . toStringData . Just

-- | An non-total version of 'tryConvertString', i.e. it evaluates to
-- an 'error' if 'tryConvertString' evaluates to 'Nothing'.
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

instance IsString StringData where { fromString = toStringData . Just; }

instance ToStringData String where
  toStringData = maybe StringUndefined (StringVector . UVec.fromList)
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
    StringBytes   a -> Just $ chr . fromIntegral <$> Bytes.unpack a
    StringByteVec a -> Just $ chr . fromIntegral <$> UVec.toList (unwrapByteVector a)
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

instance FoldableString Strict.Text where
  foldString = Strict.foldl

instance FoldableString ByteString where
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
textFromString :: (StringLength str, FoldableString str) => str -> Strict.Text
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

stringFillCharBuffer
  :: FoldableString str
  => UMVec.IOVector Char -> VectorIndex -> str -> IO VectorSize
stringFillCharBuffer mvec from str =
  let len = UMVec.length mvec in
  let copyCount = len - from in
  if copyCount <= 0 then return 0 else
  runContT
  ( callCC $ \ halt ->
    foldString
    (\ prev c ->
      prev >>= \ i ->
      if i >= len then halt i else
      UMVec.write mvec i c >>
      (pure $! i + 1)
    )
    (pure from)
    str
  )
  return

----------------------------------------------------------------------------------------------------

-- | This data type contains a string read from a character string and possibly terminated by a line
-- breaking character. If there is a line breraking character, it is converted to '\n' and the
-- actual 'LineBreakSymbol' is associated with it.
--
-- A 'TextLine' by default will encode strings as 'ByteVector's, but will automatically detect UTF8
-- encoded text, and if any character @c@ such that @ord c > 127@ exist in the stream, the internal
-- buffer is converted from 'ByteVector' to @('UVector' 'Char')@.
--
-- Getting the length of a 'TextLine' value is guaranteed to be an @O(1)@ operation.
--
-- The @tags@ type variable allows you to store annotation data along with this string, which is a
-- feature used more by the "VecEdit.Text.Editor" module for storing intermediate parser states
-- when analyzing source code, or attaching text properties like colors or font faces.
data TextLine tags
  = TextLine
    { theTextLineBreak :: !LineBreakSymbol
    , theTextLineData  :: !StringData
    , theTextLineTags  :: !(Maybe tags)
    }
  deriving (Eq, Functor)

instance Show tags => Show (TextLine tags) where
  show (TextLine{theTextLineBreak=lbrk,theTextLineData=str,theTextLineTags=tags}) =
    "(" <> show str <> " " <> show (show lbrk) <> " " <> show tags <> ")"

instance StringLength (TextLine tags) where
  stringLength (TextLine{theTextLineBreak=lbrk,theTextLineData=str}) =
    lineBreakSize lbrk + stringLength str

instance FoldableString (TextLine tags) where
  foldString f fold str =
    foldl f
    (foldString f fold $ theTextLineData str)
    (show $ theTextLineBreak str)

textLineTags :: Lens' (TextLine tags) (Maybe tags)
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

textLineBreakSymbol :: Lens' (TextLine tags) LineBreakSymbol
textLineBreakSymbol = lens theTextLineBreak $ \ a b -> a{ theTextLineBreak = b }

textLineIsUndefined :: TextLine tags -> Bool
textLineIsUndefined = theTextLineData >>> \ case { StringUndefined -> True; _ -> False; }

undefinedTextLine :: TextLine tags
undefinedTextLine =
  TextLine
  { theTextLineBreak = NoLineBreak
  , theTextLineData  = StringUndefined
  , theTextLineTags  = Nothing
  }

emptyTextLine :: LineBreakSymbol -> TextLine tags
emptyTextLine sym =
  TextLine
  { theTextLineBreak = sym
  , theTextLineData  = StringVector UVec.empty
  , theTextLineTags  = Nothing
  }

-- | If you just so happen to have an oddly specific 'GapBuffer', of the unboxed mutable
-- 'UMVec.MVector' type that also happens to be filled with 'Char' values (e.g. in the context of
-- the 'VecEdit.Text.Editor.EditLine' function), then you can extract a 'TextLine' from this
-- 'GapBuffer' using this function.
fuseIntoTextLine
  :: LineBreakSymbol
  -> Maybe tags
  -> GapBuffer UMVec.MVector Char (TextLine tags)
fuseIntoTextLine lbrk tags =
  fuseBuffer >>= \ str ->
  pure TextLine
  { theTextLineBreak = lbrk
  , theTextLineData = StringVector str
  , theTextLineTags = tags
  }

-- | Default buffer size for the 'hReadLines' function.
hReadLineBufferSize :: Int
hReadLineBufferSize = 8192

-- | Create a 'TextLine' value by cutting a 'StringData' value from a 'GapBuffer' function context
-- with an unboxed buffer of type @('UMVec.IOVector' 'Word8')@, it is assumed the character encoding
-- of the bytes is UTF8. Only characters before the 'beforeCursor' are included so evaluating
-- 'shiftCursor' might be necessary to include bytes after the 'afterCursor'. The first 'Bool'
-- argument indicates whether 'StringData' of type unboxed @('UVec.Vector' 'Word8')@ should be
-- constructed.
--
--  * passing 'True' will indicate that no re-encoding should occur, a 'ByteVector' will be
--    constructed keeping the bytes encoded as UTF8 meaning some characters may take up more than 1
--    byte of space.
--
--  * passing 'False' will indicate that re-encoding should occur, the 'ByteVector' will be copied
--    into an unboxed 'UVec.Vector' of 'Char' elements.
--
-- This function does not care whether there are line breaking symbols in the 'GapBuffer', it trusts
-- that whatever filled the 'GapBufferState' prior to being called was honest about not placing line
-- breaking characters into the buffer.
cutUTF8StringData :: Bool -> GapBuffer UMVec.MVector Word8 StringData
cutUTF8StringData w8 =
  ByteVector <$> resetCursor >>= \ str ->
  if w8 then pure $ StringByteVec str else
  use currentCursor >>= \ len ->
  withNewGapBuffer Nothing len
  (\ _st -> do
    foldString (\ prev c -> prev >> pushItem Before c) (pure ()) str
    resetCursor
  ) >>= \ case
    Left  err -> rethrowErrorInfo err
    Right str -> pure $ StringVector str
  where
  resetCursor = do
    len <- use currentCursor
    currentRange  .= Range 0 len
    currentCursor .= 0
    freezeCurrentRange

-- | Like 'cutUTF8StringData' but takes a 'LineBreakSymbol' and constructs a 'TextLine'. This should
-- be used by the 'hReadLines' function to construct a 'TextLine'.
cutUTF8TextLine :: LineBreakSymbol -> Bool -> GapBuffer UMVec.MVector Word8 (TextLine tags)
cutUTF8TextLine lbrk w8 =
  cutUTF8StringData w8 >>= \ str ->
  pure TextLine
  { theTextLineBreak = lbrk
  , theTextLineData  = str
  , theTextLineTags  = Nothing
  }

----------------------------------------------------------------------------------------------------

-- | A class of stateful data types that can produce a stream 'Word8' byte values.
class IOByteStream stream where
  streamEnded :: stream -> IO Bool
  streamNext :: stream -> IO Word8

instance IOByteStream Handle where
  streamEnded = hIsEOF
  streamNext = fmap (fromIntegral . ord) . hGetChar 

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
newtype EditorStream = EditorStream (IORef (EditorState UMVec.MVector Word8))

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

-- | Construct an 'EditorStream' which can be used as an argument to 'streamReadLines'.
newEditorStream :: EditorState UMVec.MVector Word8 -> IO EditorStream
newEditorStream st = EditorStream <$> newIORef (currentCursor .~ 0 $ st)

-- | Construct an 'IORef' containing a strict 'ByteStream', so that the 'IORef' can be used as an
-- argument to 'streamReadLines'.
streamByteString :: ByteString -> IO (IORef ByteString)
streamByteString = newIORef

-- | Construct an 'IORef' containing a lazy 'ByteStream', so that the 'IORef' can be used as an
-- argument to 'streamReadLines'.
streamLazyByteString :: LazyBytes.ByteString -> IO (IORef LazyBytes.ByteString)
streamLazyByteString = newIORef

----------------------------------------------------------------------------------------------------

-- | This function type is used for reading through 'IOByteStream's and building 'GapBuffer's full
-- of 'TextLine's. It instantiates 'MonadCont' and most of the looping APIs in this module pass a
-- halting function of type 'HaltReadLines' to their continuation so that the continuation can
-- implement logic that decides when to halt the read loop early.
--
-- Functions of this type are used as continuations by stream reader functions such as 'hReadLines'
-- or 'streamReadLines'. These stream reader functions loop until the 'checkForEnd' condition is
-- met, and pass each line of characters read from the stream to a continuation function of this
-- type.
newtype ReadLines fold a
  = ReadLines
    { unwrapReadLines ::
        ExceptT
        EditTextError
        ( ContT
          (Either EditTextError ())
          ( StateT
            (ReadLinesState fold)
            IO
          )
        )
        a
    }
  deriving
  (Functor, Applicative, Monad, MonadIO, MonadCont, MonadError EditTextError)

-- | Functions of this type are constructed by stream reader functions such as 'hReadLines' or
-- 'streamReadLines' evaluating 'callCC' prior to entering the stream reading loop, and are passed
-- to the loop continuation functions of type 'ReadLines' so that these continuation functions may
-- evaluate the 'HaltReadLines' function to halt the stream reading loop.
type HaltReadLines fold void = fold -> ReadLines fold void

-- | This is a 'GapBuffer' function type that buffers characters into an unboxed mutable
-- 'UMVec.MVector'.
type Word8GapBuffer = GapBuffer UMVec.MVector Word8

-- | This is the state of a 'Word8GapBuffer' function type, which includes an unboxed mutable
-- 'UMVec.MVector' for buffering characters.
type Word8GapBufferState = GapBufferState UMVec.MVector Word8

-- | This is the stateful context of the 'ReadLines' function type.
data ReadLinesState fold
  = ReadLinesState
    { theLineIsAsciiOnly    :: !Bool
      -- ^ Indicates whether the 'ord' of all characters values is less or equal to 127. This tells
      -- us whether we want to encode the line as a @('UVector' 'Word8')@ or a @('UVector' 'Char').
    , theReadLinesState     :: !fold
    , theReadLinesGapBuffer :: !(GapBufferState UMVec.MVector Word8)
    }

instance MonadState fold (ReadLines fold) where
  state f =
    ReadLines $ lift $ state $ \ st ->
    let (a, fold) = f (st ^. readLinesState) in
    (a, (readLinesState .~ fold) st)

-- | Returns a boolean symbol indicating whether or not a non-ascii (upper UTF-8) symbol has been
-- read since reading the last line break character. This indicates that the current string being
-- constructed may need to be copied to a string containing 16 or 32-bit wide characters, or else
-- formatted as UTF-8 or UTF-16 rather than simply Char8.
isAsciiOnly :: ReadLines fold Bool
isAsciiOnly = ReadLines $ lift $ lift $ use lineIsAsciiOnly

lineIsAsciiOnly :: Lens' (ReadLinesState fold) Bool
lineIsAsciiOnly = lens theLineIsAsciiOnly $ \ a b -> a{ theLineIsAsciiOnly = b }

readLinesState :: Lens' (ReadLinesState fold) fold
readLinesState = lens theReadLinesState $ \ a b -> a{ theReadLinesState = b }

readLinesGapBuffer :: Lens' (ReadLinesState fold) (GapBufferState UMVec.MVector Word8)
readLinesGapBuffer = lens theReadLinesGapBuffer $ \ a b -> a{ theReadLinesGapBuffer = b }

runReadLinesIO
  :: ReadLines fold a
  -> Word8GapBufferState
  -> fold
  -> IO (Either EditTextError (), ReadLinesState fold)
runReadLinesIO (ReadLines (ExceptT f)) gapbuf fold =
  runStateT (runContT (fmap (const ()) <$> f) pure) ReadLinesState
  { theLineIsAsciiOnly    = True
  , theReadLinesState     = fold
  , theReadLinesGapBuffer = gapbuf
  }

runReadLines
  :: ReadLines fold a
  -> fold
  -> Word8GapBuffer (Either EditTextError (), fold)
runReadLines (ReadLines (ExceptT f)) fold = do
  gapbuf <- get
  (result, rlst) <-
    liftIO $
    runStateT
    (runContT (fmap (const ()) <$> f) pure)
    ReadLinesState
    { theLineIsAsciiOnly    = True
    , theReadLinesState     = fold
    , theReadLinesGapBuffer = gapbuf
    }
  put (rlst ^. readLinesGapBuffer)
  pure (result, rlst ^. readLinesState)

updateLineIsAsciiOnly :: (Bool -> Bool) -> ReadLines fold ()
updateLineIsAsciiOnly = ReadLines . lift . (lineIsAsciiOnly %=)

-- | Lift a 'Word8GapBuffer' function into the 'ReadLines' function.
readLinesLiftGapBuffer :: Word8GapBuffer a -> ReadLines fold a
readLinesLiftGapBuffer f =
  ReadLines $ do
    (result, gapbuf) <-
      lift (lift $ use readLinesGapBuffer) >>=
      liftIO . runGapBuffer f
    lift $ lift $ readLinesGapBuffer .= gapbuf
    (throwError . EditTextError ||| pure) result

-- | This function reads a file 'Handle' similar to how 'hGetContents' works, but buffering all
-- characters into a 'StringData' structure until a line breaking character is found. Once the line
-- break character is received, or the end of the file 'Handle' stream is reached, the given
-- continuation is evaluated with the newly created 'StringData' value. Line breaks include
-- @"\\n\\r"@, @"\\r\\n"@, @"\n"@, @"\r"@, @"\f"@, @"\v"@, and @"\0"@ (null character).
--
-- The 'Handle' will be set to 'char8' encoding and will have buffering disabled since a buffer
-- already exists.
--
-- Pass an 'Int' value as a recommended buffer size. If 'Nothing' is given, a default value is used.
hReadLines
  :: Word8GapBufferState
  -> Handle
  -> (HaltReadLines fold void -> TextLine tags -> ReadLines fold ())
  -> fold
  -> IO (Either EditTextError (), fold)
hReadLines buf handle f fold = do
  hSetByteStreamMode handle
  streamReadLines lineBreak_LF buf handle f fold

-- | This function calls 'byteStreamToLines' with 'cutUTF8TextLine' as the fold function. You supply
-- your own fold function which gets the output of each 'cutUTF8TextLine', so you can more easily
-- fold over 'TextLine' data values using an 'IO' function, rather than the slightly more difficult
-- task of folding over 'LineBreakSymbol's in a function of type 'GapBuffer'.
streamReadLines
  :: IOByteStream handle
  => TokenizerTable LineBreakSymbol LineBreakerState
  -> Word8GapBufferState
  -> handle
  -> (HaltReadLines fold void -> TextLine tags -> ReadLines fold ())
  -> fold
  -> IO (Either EditTextError (), fold)
streamReadLines table buf h f =
  byteStreamToLines table buf h $ \ halt lbrk ->
  ReadLines (lift $ use lineIsAsciiOnly) >>= \ w8 ->
  readLinesLiftGapBuffer (cutUTF8TextLine lbrk w8) >>=
  f halt
{-# INLINE streamReadLines #-}

-- | It is important to set a 'Handle' to byte stream mode when reading it with 'hReadLines' or
-- 'byteStreamToLines'.
hSetByteStreamMode :: Handle -> IO ()
hSetByteStreamMode h = do
  hSetEncoding h char8
  hSetBuffering h NoBuffering

-- | This is the most general line breaking function. It takes an arbitrary 'IOByteStream' (which is
-- usally a 'Handle'), and an arbitrary 'TokenizerTable' for doing the line splitting (which is
-- usually 'lineBreak_LF' on UNIX-like systems, and 'lineBreak_CRLF' on Windows systems). It could
-- also be set to 'lineBreak_NUL' or 'allLineBreaks' (to break on @'\FF'@ and @'\VT'@ characters as
-- well), or any 'TokenizerTable' of your choosing.
byteStreamToLines
  :: IOByteStream handle
  => TokenizerTable LineBreakSymbol LineBreakerState
  -> Word8GapBufferState
  -> handle
  -> (HaltReadLines fold void -> LineBreakSymbol -> ReadLines fold ())
  -> fold
  -> IO (Either EditTextError (), fold)
byteStreamToLines table gapbuf h f fold = begin where
  begin =
    fmap (fmap (^. readLinesState)) $
    runReadLinesIO (callCC loop) gapbuf fold
  loop halt =
    liftIO (streamEnded h) >>= \ done ->
    if done then final halt NoLineBreak else
    liftIO (streamNext h) >>=
    checkForEnd halt
  checkForEnd halt c8 =
    let (lbrk, st) = tokTableLookup table (chr $ fromIntegral c8) in
    case st of
      CharLineBreakNext   -> next halt lbrk
      CharLineBreak       -> cutLine halt lbrk >> loop halt
      CharNotLineBreaking -> do
        readLinesLiftGapBuffer (pushItem Before $! c8)
        updateLineIsAsciiOnly (&& (0 == (c8 .&. 0x80)))
        loop halt
  next halt lbrk =
    liftIO (streamEnded h) >>= \ done ->
    if done then final halt lbrk else
    liftIO (streamNext h) >>= \ c8 ->
    let c = chr $ fromIntegral c8 in
    case lbrk of
      LineBreakLF | c == '\CR' -> cutLine halt LineBreakLFCR >> loop halt
      LineBreakCR | c == '\LF' -> cutLine halt LineBreakCRLF >> loop halt
      _                        -> cutLine halt lbrk >> checkForEnd halt c8
  final halt lbrk = do
    cur <- readLinesLiftGapBuffer (use currentCursor)
    when (cur > 0) (cutLine halt lbrk)
    Right <$> get
  cutLine halt = f (halt . Right)
