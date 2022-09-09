module VecEdit.Text.Line.Break
  ( -- * Line Break Behavior
    --
    -- The line break behavior of the 'TextBuffer' can be programmed to behave differently from the
    -- ordinary default behavior of breaking input strings on the @'\n'@ character.
    LineBreakSymbol(..), lineBreakSize,
    LineBreakerState(..),
    allLineBreaks, allLineBreaksNext, lineBreak_LF, lineBreak_NUL,
  ) where

import VecEdit.Text.TokenizerTable (TokenizerTable, tokTableFromList)

----------------------------------------------------------------------------------------------------

data LineBreakSymbol
  = NoLineBreak
  | LineBreakLF
  | LineBreakCR
  | LineBreakNUL
  | LineBreakLFCR
  | LineBreakCRLF
  | LineBreakFF
  | LineBreakVT
  deriving (Eq, Ord, Bounded, Enum)

instance Show LineBreakSymbol where
  show = \ case
    NoLineBreak   -> ""
    LineBreakLF   -> "\LF"
    LineBreakCR   -> "\CR"
    LineBreakNUL  -> "\NUL"
    LineBreakLFCR -> "\LF\CR"
    LineBreakCRLF -> "\CR\LF"
    LineBreakFF   -> "\FF"
    LineBreakVT   -> "\VT"

instance Read LineBreakSymbol where
  readsPrec _ = \ case
    ""              -> [(NoLineBreak, "")]
    '\LF':'\CR':str -> [(LineBreakLFCR, str)]
    '\CR':'\LF':str -> [(LineBreakCRLF, str)]
    '\LF':str       -> [(LineBreakLF, str)]
    '\CR':str       -> [(LineBreakCR, str)]
    '\NUL':str      -> [(LineBreakNUL, str)]
    '\FF':str       -> [(LineBreakFF, str)]
    '\VT':str       -> [(LineBreakVT, str)]
    _               -> []

lineBreakSize :: Num n => LineBreakSymbol -> n
lineBreakSize = \ case
  NoLineBreak   -> 0
  LineBreakLF   -> 1
  LineBreakCR   -> 1
  LineBreakNUL  -> 1
  LineBreakLFCR -> 2
  LineBreakCRLF -> 2
  LineBreakFF   -> 1
  LineBreakVT   -> 1

----------------------------------------------------------------------------------------------------

data LineBreakerState
  = CharNotLineBreaking
  | CharLineBreak
  | CharLineBreakNext
  deriving (Eq, Show, Bounded, Enum)

-- | This table breaks on all possible line break symbols listed in the 'LineBreakSymbol'.
allLineBreaks :: TokenizerTable LineBreakSymbol LineBreakerState
allLineBreaks =
  tokTableFromList
  NoLineBreak
  CharNotLineBreaking
  [ ('\NUL', LineBreakNUL, CharLineBreak)
  , ('\LF', LineBreakLF, CharLineBreakNext)
  , ('\CR', LineBreakCR, CharLineBreakNext)
  , ('\FF', LineBreakFF, CharLineBreak)
  , ('\VT', LineBreakVT, CharLineBreak)
  ]

-- | This table is used if a 'CharLineBreakNext' state occurs.
allLineBreaksNext :: TokenizerTable LineBreakSymbol LineBreakerState
allLineBreaksNext =
  tokTableFromList
  NoLineBreak
  CharNotLineBreaking
  [ ('\CR', LineBreakLFCR, CharLineBreak)
  , ('\LF', LineBreakCRLF, CharLineBreak)
  ]

-- | This table breaks on the line break @'\LF'@ characters only, which is the default behavior on
-- UNIX-like systems.
lineBreak_LF :: TokenizerTable LineBreakSymbol LineBreakerState
lineBreak_LF =
  tokTableFromList
  NoLineBreak
  CharNotLineBreaking
  [('\LF', LineBreakLF, CharLineBreak)]

-- | This table breaks lines only on @'\NUL'@ (zero) characters, which can be useful when taking
-- reading a byte stream encoded a list of null-terminated strings, for example, when implementing
-- the functionality of the ~xargs~ program. __WARNING:__ if you open a binary file with lots of
-- zeros (and most binary files have lots of zero padded data), you will get lots and lots of empty
-- strings created by this 'TokenizerTable', which could eat up a lot of memory by mistake.
lineBreak_NUL :: TokenizerTable LineBreakSymbol LineBreakerState
lineBreak_NUL =
  tokTableFromList
  NoLineBreak
  CharNotLineBreaking
  [('\NUL', LineBreakNUL, CharLineBreak)]
