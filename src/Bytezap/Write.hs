-- safe module, only export the safe bits (no @Write(..)@!!)
module Bytezap.Write
  ( Write, runWriteBS, runWriteBSUptoN, prim
  ) where

import Bytezap.Poke qualified as P
import Raehik.Compat.Data.Primitive.Types
import GHC.Exts

import Data.ByteString qualified as BS

-- | A 'Poke' with the associated size it pokes.
data Write s = Write { size :: Int, poke :: P.Poke s }

-- | Sequence the 'Poke's, sum the sizes.
instance Semigroup (Write s) where
    -- TODO feels like this might be INLINE[1] or even INLINE[0]?
    {-# INLINE (<>) #-}
    Write ll lp <> Write rl rp = Write (ll + rl) (lp <> rp)

-- | The empty 'Write' is the empty 'Poke', which writes zero bytes.
instance Monoid (Write s) where
    {-# INLINE mempty #-}
    mempty = Write 0 mempty

runWriteBS :: Write RealWorld -> BS.ByteString
runWriteBS = runWriteWith P.unsafeRunPokeBS

runWriteBSUptoN :: Write RealWorld -> BS.ByteString
runWriteBSUptoN = runWriteWith P.unsafeRunPokeBSUptoN

-- | Helper for writing 'Write' runners.
--
-- TODO still needed with no Pokeable or not? idk
runWriteWith :: forall a s. (Int -> P.Poke s -> a) -> Write s -> a
runWriteWith runPoke (Write size poke) = runPoke size poke

prim :: forall a s. Prim' a => a -> Write s
prim a = Write (sizeOf (undefined :: a)) (P.prim a)
