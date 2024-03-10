{-# LANGUAGE CPP #-}

-- this gets us our WORD_SIZE_IN_BITS CPP macro
#include "MachDeps.h"

module Bytezap.Poke.KnownLen where

import Bytezap.Poke
import GHC.TypeNats
import Data.ByteString qualified as BS
import GHC.Exts
import Util.TypeNats ( natValInt )
import Data.Word
import Data.Int

import Bytezap.Pokeable qualified as P

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

w8   :: P.Pokeable s (ptr :: TYPE rr) => Word8  -> PokeKnownLen 1 s ptr
w8   = PokeKnownLen . P.w8

w16  :: P.Pokeable s (ptr :: TYPE rr) => Word16 -> PokeKnownLen 2 s ptr
w16  = PokeKnownLen . P.w16

w32  :: P.Pokeable s (ptr :: TYPE rr) => Word32 -> PokeKnownLen 4 s ptr
w32  = PokeKnownLen . P.w32

w64  :: P.Pokeable s (ptr :: TYPE rr) => Word64 -> PokeKnownLen 8 s ptr
w64  = PokeKnownLen . P.w64

i8   :: P.Pokeable s (ptr :: TYPE rr) =>  Int8  -> PokeKnownLen 1 s ptr
i8   = PokeKnownLen . P.i8

i16  :: P.Pokeable s (ptr :: TYPE rr) =>  Int16 -> PokeKnownLen 2 s ptr
i16  = PokeKnownLen . P.i16

i32  :: P.Pokeable s (ptr :: TYPE rr) =>  Int32 -> PokeKnownLen 4 s ptr
i32  = PokeKnownLen . P.i32

i64  :: P.Pokeable s (ptr :: TYPE rr) =>  Int64 -> PokeKnownLen 8 s ptr
i64  = PokeKnownLen . P.i64

#if WORD_SIZE_IN_BITS == 64
int  :: P.Pokeable s (ptr :: TYPE rr) =>  Int   -> PokeKnownLen 8 s ptr
word :: P.Pokeable s (ptr :: TYPE rr) => Word   -> PokeKnownLen 8 s ptr
#elif WORD_SIZE_IN_BITS == 32
int  :: P.Pokeable s (ptr :: TYPE rr) =>  Int   -> PokeKnownLen 4 s ptr
word :: P.Pokeable s (ptr :: TYPE rr) => Word   -> PokeKnownLen 4 s ptr
#else
#error unsupported platform (not 32/64 bit)
#endif
int  = PokeKnownLen . P.int
word = PokeKnownLen . P.word
