-- | This  module provides  APIs for  modeling a stream  of bytes.  The 'IOByteStream'  typeclass is
-- defined with  the 'System.IO.Handle'  data type  as an  instance of  it. It  is also  possible to
-- construct streams around pure data structures such as lazy 'Data.ByteString.Lazy.ByteString's.
module VecEdit.Text.Stream
  ( -- ** Constructing a byte stream
    IOByteStream(..), EditorStream,
    newEditorStream, streamByteString, streamLazyByteString,
    -- ** Folding and editing over a stream
    EditStream(..), EditStreamState, HaltEditStream, HandleUTFDecodeError,
    newEditStreamState, editStreamState, runEditStream,
    hFoldLines, streamFoldLines, hSetByteStreamMode,
    byteStreamDecode, hEditStreamBufferSize,
  )
  where

import VecEdit.Text.Internal
