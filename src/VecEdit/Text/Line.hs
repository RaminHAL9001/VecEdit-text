-- | This  module provides the 'TextLine'  data type, which is  a string that is  guaranteed to have
-- exactly zero or one line  breaking symbols, and the line break is guaranteed to  be at the end of
-- the string. This data  type is most useful in the  context of a 'VecEdit.Text.Editor.TextBuffer',
-- the data model of which is  a 'VecEdit.Vector.Editor.GapBuffer.GapBuffer' containing zero or more
-- 'TextLine's.
--
-- This module also provides type classes for working with strings of various encodings.
--
--  - @String@ -- lists of characters
--
--  - Strict and lazy 'Strict.Text' -- a sequence 16-bit wide words encoding a string of UTF-16
--  - 'Char's.
--
--  - Strict and lazy 'Char 'CBytes.ByteString' -- a  sequence of 8-bit wide words encoding a string
--    of ASCII-only (no UTF) characters.
--
--  - Strict and lazy UTF-8 encoded 'UTF8String.ByteString'.
--
--  - Unboxed 'UVec.Vector's of 8-bit 'Char' values (ASCII-only, no UTF).
--
--  - Unboxed 'UVec.Vector's of 32-bit wide UTF 'Char' values (UTF-32 encoded strings).
--
-- Sorry,  but 'UVec.Vectors'  encoding 24-bit  wide UTF  'Char' values  are not  supported as  this
-- encoding not used very often.
module VecEdit.Text.Line
  ( -- ** Lines of Text
    TextLine, theTextLineBreak, textLineData, textLineString, theTextLineTags,
    textLineTags, textLineBreakSymbol, textLineNull,
    emptyTextLine, textLineChomp, textLineContentLength,
    -- *** Internal representation of a 'TextLine'
    StringData(..), ByteVector(..), Char8, toChar8Lossy, fromChar8,
    CharVector, byteVectorSize,
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
  ) where

import VecEdit.Text.Internal
