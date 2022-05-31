module VecEdit.Text.TokenizerTable
  ( TokenizerTable, tokTableFromList, tokTableLookup, tokTableFold,
    EnumToTokenizer(..),
  ) where

import Control.Monad (forM_)

import Data.Char (ord, chr)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec

----------------------------------------------------------------------------------------------------

-- | This is a data type for defining table-based tokenizers, especially the tables to do line
-- breaking. The value @n@ must be a type in the class of 'Enum', and of 'Bounded' such that the
-- following is always true:
--
-- @
-- typeof_n_is_OK_to_use_here =
--     fromEnum (minBound :: n) >= fromEnum (minBound :: Word8) &&
--     fromEnum (maxBound :: n) <= fromEnum (maxBound :: Word8)  
-- @
data TokenizerTable token state
  = TokenizerTable
    { theTokenizerTable :: !(UVec.Vector Word8)
      -- ^ The table is a vector of enumerable states. Tokenizer tables should not have more than
      -- @2^16@ possible states, due to how states are encoded into an unboxed 'UVec.Vector'.
    , theTokenizerBound :: !Char
      -- ^ The lower bound, which offsets indicies into the vector.
    , theTokenizerDefaultToken :: !token
    , theTokenizerDefaultState :: !state
    }
  deriving (Eq, Functor)

-- | Pass a default @n@ value (a value to be used if some 'Char' indicies have no associated @n@
-- values), and then a list of pairs associating a 'Char' index with an @n@ value. The @n@ value
-- must satisfy the following law:
--
-- @
-- typeof_n_is_OK_to_use_here =
--     fromEnum (minBound :: n) >= fromEnum (minBound :: Word8) &&
--     fromEnum (maxBound :: n) <= fromEnum (maxBound :: Word8)  
-- @
--
-- If the law is not obeyed, this function evaluates to 'error'.
tokTableFromList
  :: forall state token . (Enum token, Bounded token, Enum state, Bounded state)
  => token -> state -> [(Char, token, state)] -> TokenizerTable token state
tokTableFromList defToken0 defState0 elems =
  TokenizerTable
  { theTokenizerTable =
    UVec.create
    (do v <- UMVec.new (2*spanSize)
        forM_ [0 .. spanSize-1] $ \ i -> do
          UMVec.write v (2*i)   defToken
          UMVec.write v (2*i+1) defState
        forM_ elems $ \ (ci, tok, st) -> do
          let i = ord ci - ord charLO
          UMVec.write v (2*i)   $! fromIntegral $! fromEnum tok
          UMVec.write v (2*i+1) $! fromIntegral $! fromEnum st
        return v
    )
  , theTokenizerBound = charLO
  , theTokenizerDefaultToken = defToken0
  , theTokenizerDefaultState = defState0
  }
  where
    defToken = checkDefault "token" defToken0
    defState = checkDefault "state" defState0
    chars  = (\ (c, _, _) -> c) <$> elems
    charLO = minimum chars
    charHI = maximum chars
    spanSize = ord charHI - ord charLO + 1

checkDefault :: forall def . (Enum def, Bounded def) => String -> def -> Word8
checkDefault typ def =
  if fromEnum (minBound :: def) >= fromEnum (minBound :: Word8) &&
     fromEnum (maxBound :: def) <= fromEnum (maxBound :: Word8)
  then fromIntegral (fromEnum def)
  else
    error $
    "tokTableFromList: fromEnum may return an out-of-bound value" <>
    " fromEnum (minBound::" <> typ <> ") == " <>
    show (fromEnum (minBound :: def)) <>
    " (fromEnum (maxBound::" <> typ <> ")) == " <>
    show (fromEnum (maxBound :: def))

-- | This function looks-up a 'Char' value in a 'TokenizerTable' and returns a @state@ value.
tokTableLookup
  :: (Enum token, Bounded token, Enum state, Bounded state)
  => TokenizerTable token state -> Char -> (token, state)
tokTableLookup table c =
  let deflt = (theTokenizerDefaultToken table, theTokenizerDefaultState table) in
  let vec = theTokenizerTable table in
  let ci = ord c in
  let lo = ord (theTokenizerBound table) in
  if ci < lo then deflt else
  let i = ci - lo in
  let len = UVec.length vec `div` 2 in
  if i < len then
  ( toEnum $ fromIntegral $ vec UVec.! (2*i)
  , toEnum $ fromIntegral $ vec UVec.! (2*i+1)
  )
  else deflt
{-# INLINE tokTableLookup #-}

tokTableFold
  :: (Enum token, Bounded token, Enum state, Bounded state)
  => (fold -> (Char, token, state) -> fold) -> fold -> TokenizerTable token state -> fold
tokTableFold f fold table = loop 0 fold where
  vec = theTokenizerTable table
  len = UVec.length vec `div` 2
  loop i fold =
    if i >= len then fold else
    (loop $! i + 1) $!
    f fold
    ( chr $ fromEnum i + fromEnum (theTokenizerBound table)
    , toEnum $ fromIntegral $ vec UVec.! (2*i)
    , toEnum $ fromIntegral $ vec UVec.! (2*i+1)
    )

----------------------------------------------------------------------------------------------------

-- | This class provides a function that selects a 'TokenizerTable' from a @state@ value.
class EnumToTokenizer token state | state -> token where
  nextTokenizerState :: state -> TokenizerTable token state
