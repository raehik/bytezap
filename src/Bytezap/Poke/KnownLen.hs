{-# LANGUAGE CPP #-}

-- this gets us our WORD_SIZE_IN_BITS CPP macro
#include "MachDeps.h"

module Bytezap.Poke.KnownLen where

import Bytezap.Poke
import GHC.TypeNats
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import GHC.Exts
import Util.TypeNats ( natValInt )

import Bytezap.Pokeable qualified as P
import Raehik.Compat.Data.Primitive.Types

newtype PokeKnownLen (len :: Natural) s (ptr :: TYPE rr) =
    PokeKnownLen { unPokeKnownLen :: Poke s ptr }

mappend'
    :: Semigroup (Poke s ptr)
    => PokeKnownLen n     s (ptr :: TYPE rr)
    -> PokeKnownLen m     s ptr
    -> PokeKnownLen (n+m) s ptr
mappend' (PokeKnownLen l) (PokeKnownLen r) = PokeKnownLen (l <> r)

mempty' :: Monoid (Poke s ptr) => PokeKnownLen 0 s (ptr :: TYPE rr)
mempty' = PokeKnownLen mempty

runPokeKnownLenBS
    :: forall n. KnownNat n
    => PokeKnownLen n RealWorld Addr#
    -> BS.ByteString
runPokeKnownLenBS (PokeKnownLen p) = unsafeRunPokeBS (natValInt @n) p

runPokeKnownLenSBS
    :: forall n. KnownNat n
    => (forall s. PokeKnownLen n s (MutableByteArray# s))
    -> SBS.ShortByteString
runPokeKnownLenSBS p = unsafeRunPokeSBS (natValInt @n) (unPokeKnownLen p)

prim
    :: (P.Pokeable (ptr :: TYPE rr), Prim' a)
    => a
    -> PokeKnownLen (SizeOf a) (P.PS ptr) ptr
prim = PokeKnownLen . P.prim
