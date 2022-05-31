# VecEdit-text

This package provides some basic text editor functionality around the
`VecEdit` package. `VecEdit` is a small library that provides some
**unabashedly stateful** monadic function types for editing mutable
vectors. The main use case is rapid prototyping of mutable vector
algorithms in a GHCI session.

The internal representation of a text buffer is a mutable vector of
strings, each string containing a line terminator except for the final
string which may or may not have a line terminator.

All of the APIs in this module make use of a separate data type for
indexing lines and chacaters that are 1-indexed: `TextPoint`,
`LineIndex`, and `CharIndex`. The hope is that the type checker can
help prevent off-by-1 errors that can occur when indexing a `Vector`
data type using an integer value that expects "line 1" to be the 0th
element in the vector.

## Breif overview of useful APIs

### `VecEdit.Text.String`

This module provides the immutable `StringData` data type, which is a
sum type representing several different data structures that can
encode a string.

- `Data.Text`
- `Data.Text.Lazy`
- `Data.ByteString`
- A byte vector `(Data.Vector.Unboxed.Vector Char8)`
- A char vector `(Data.Vector.Unboxed.Vector Char)`
- `undefined`, which is similar to `Nothing` and is used to fill the
  gap of the text buffer.

There are also classes for converting between all of these string types:

- `FromStringData`
- `ToStringData`

All the string types provided in this module instantiate the above two
classes in the most efficient way I know how, so it is possible to
convert between any string data structure.

### `VecEdit.Text.Editor`

This module provides `EditText` monad, which is a state monad over a
`GapBuffer` (from the `VecEdit` package) that can be filled with
immutable lines of `StringData`.

This module also provides an `EditLine`, which is a state monad over
an unboxed `GapBuffer` of characters that serves as a line editor. The
line editor can be filled with characters, and then frozen and pushed
to the cursor position in the text buffer.

Every `EditText` monad also contains a line editor state, and can lift
the `EditLine` monad.

Useful APIs include:

- `insertChar`
- `flushLine`
- `newline`
- `insertString`
- `cursorToEnd`

### `VecEdit.Text.LineBreak`

Represents the most common way of encoding line breaks.

- `\CR\LF`
- `\LF\CR`
- `\CR`
- `\LF`
- `\FF`
- `\VT`
- and `\NUL` should be used with care, it can create millions of empty
  lines of text if accidentally reading a binary file with many zero
  values. This exists so the output of programs like `find -print0` or
  `printenv -0` can be parsed as lines of text.

### `VecEdit.Text.TokenizerTable`

This module provides a `TokenizerTable` data type that can be used
when filling an `EditText` buffer with lines of text from a
`System.IO.Handle`. Characters can be looked-up in a `TokenizerTable`
to decide if the character is a `LineBreak`-ing character that should
committed the string to the text buffer. Different tables are provided
based on which `LineBreak` characters are considered valid. A line
breaker that breaks on `\CR` or `\LF` or `\VT` should be equally
efficient as a line breaker that breaks only on `\LF`.
