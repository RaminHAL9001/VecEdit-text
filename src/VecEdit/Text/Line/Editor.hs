module VecEdit.Text.Line.Editor
  ( -- ** The 'LineEditor' typeclass
    LineEditor
      ( liftEditLine, countLines, pushLine, popLine, insertLineAtEnd, insertLineHere,
        copyLine, cutLine, copyBuffer, clearBuffer, shiftCursor,
        pushChar, popChar, tryPopChar, countChars
      ),
    defaultEditLineLiftResult,
    insertString, insert, joinLine, newline, lineBreak, copyBufferClear,
    -- ** The 'EditLine' function type
    EditLine(..), LineBuffer, EditLineState, newEditLineState, runEditLine, streamEditor,
    EditLineResult(..), editLineUTFWeight, onEditLineState, catchEditLineResult,
    maxCharIndex, minCharIndex, columnNumber,
    editLineTokenizer, editLineBreakSymbol, editLineLiftResult,
  ) where

import VecEdit.Text.Internal
