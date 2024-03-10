{-# LANGUAGE UndecidableInstances #-} -- for levity-polymorphic instances

-- safe module, only export the safe bits (no @Write(..)@!!)
module Bytezap.Write ( Write, runWriteBS, runWriteBSUptoN, runWriteSBS ) where

import Bytezap.Poke
import GHC.Exts

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS

-- | A 'Poke' with the associated size it pokes.
data Write s (ptr :: TYPE rr) = Write { size :: Int, poke :: Poke s ptr }

-- | Sequence the 'Poke's, sum the sizes.
instance Semigroup (Poke s ptr) => Semigroup (Write s (ptr :: TYPE rr)) where
    -- TODO feels like this might be INLINE[1] or even INLINE[0]?
    {-# INLINE (<>) #-}
    Write ll lp <> Write rl rp = Write (ll + rl) (lp <> rp)

-- | The empty 'Write' is the empty 'Poke', which writes zero bytes.
instance Monoid (Poke s ptr) => Monoid (Write s (ptr :: TYPE rr)) where
    {-# INLINE mempty #-}
    mempty = Write 0 mempty

-- | Serialize via ByteString and 'show' the result.
--instance Show (Write s ptr) where showsPrec p = showsPrec p . runWriteBS

runWriteBS :: Write RealWorld Addr# -> BS.ByteString
runWriteBS = runWriteWith unsafeRunPokeBS

runWriteBSUptoN :: Write RealWorld Addr# -> BS.ByteString
runWriteBSUptoN = runWriteWith unsafeRunPokeBSUptoN

runWriteSBS :: (forall s. Write s (MutableByteArray# s)) -> SBS.ShortByteString
runWriteSBS w = unsafeRunPokeSBS (size w) (poke w)

-- | Helper for writing 'Write' runners.
runWriteWith
    :: forall {rr} (ptr :: TYPE rr) a s
    .  (Int -> Poke s ptr -> a)
    -> Write s ptr
    -> a
runWriteWith runPoke (Write size poke) = runPoke size poke
