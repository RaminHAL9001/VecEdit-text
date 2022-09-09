-- |   This    module   provides    a   UTF-8    decoder.   It   is    used   internally    to   the
-- 'VecEdit.Text.Stream.streamFoldLines' function, and  functions related to it, however  the API is
-- exposed here should it be useful elsewhere.
--
-- The decoder is  a set of simple  pure functions wrapped up in  a pure 'Control.Monad.State.State'
-- function type. You can single-step the decoder by feeding  bytes into it one at a time, each time
-- evaluating the 'utf8Decoder' function and inspecting the 'UTF8Decoder' state.
module VecEdit.Text.UTF8
  ( UTF8Decoder(..), utf8Decoder, utf8DecoderPushByte,
    UTF8DecodeTakeChar, UTF8DecodeStep, UTF8DecodeTakeError, utf8ErrorDecompose,
  ) where

import VecEdit.Text.Internal
