{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

-- this gets us our word size and maybe endianness CPP macros
#include "MachDeps.h"

module Bytezap.Pokeable where

import Bytezap.Poke
import GHC.Exts
import Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops

import Data.ByteString.Internal qualified as BS
import GHC.ForeignPtr ( ForeignPtr(..) )
import Raehik.Compat.Data.Primitive.Types

{- | Types that support direct, low-level "poking" operations, where values are
     pointers of some sort.

GHC provides two sets of primops for moving primitive types between memory:

* via 'Addr#', for accessing non-GC (garbage collected) addresses
* via '(Mutable)ByteArray#', for accessing GCed memory (pinned or unpinned)

'BS.ByteString's are wrappers over 'ForeignPtr's storing an 'Addr#', and are not
GC-managed for various reasons. This is the natural type to target when
performing low-level serialization. But let us not forget the humble
'SBS.ShortByteString', which wrap 'ByteArray#'s. These are GCed. This type class
enables writing a single "poke program" which need only be specialized when
running the program.

Notably, 'SBS.ShortByteString's may be created in 'Control.Monad.ST.ST'.
-}
class Monoid (Poke (PS ptr) ptr) => Pokeable (ptr :: TYPE rr) where
    -- | State token type.
    --
    -- This allows us to run in ST if our pointer type supports it (e.g.
    -- @'MutableByteArray#' s'@.
    type PS ptr

    -- | Poke a type via its 'Prim'' instance.
    prim :: Prim' a => a -> Poke (PS ptr) ptr

    -- | Poke a 'BS.ByteString'.
    byteString :: BS.ByteString        -> Poke (PS ptr) ptr

    -- | Poke a 'ByteArray#' starting from the given 'Int' offset.
    byteArray# :: ByteArray#    -> Int -> Poke (PS ptr) ptr

-- | 'prim' with reordered types for convenient visible type application
prim'
    :: forall {rr} a (ptr :: TYPE rr)
    .  (Prim' a, Pokeable ptr) => a -> Poke (PS ptr) ptr
prim' = prim

instance Pokeable Addr# where
    type PS Addr# = RealWorld

    prim :: forall a s. Prim' a => a -> Poke s Addr#
    prim a = Poke $ \base# os# s# ->
        case writeWord8OffAddrAs# base# os# a s# of
          s'# -> (# s'#, os# +# sizeOf# (undefined :: a) #)
    {-# INLINE prim #-}

    -- | This function forces us to set our state token to 'RealWorld'.
    --   I don't really get why! But seems sensible enough.
    byteString (BS.BS (ForeignPtr p# c) (I# len#)) = Poke $ \base# os# s# ->
        case copyAddrToAddrNonOverlapping# p# (base# `plusAddr#` os#) len# s# of
          s'# -> case touch# c s'# of s''# -> (# s''#, os# +# len# #)
    {-# INLINE byteString #-}

    byteArray# ba# (I# baos#) = Poke $ \base# os# s# ->
        let len# = sizeofByteArray# ba#
        in  case copyByteArrayToAddr# ba# baos# (base# `plusAddr#` os#) len# s# of
              s'# -> (# s'#, os# +# len# #)
    {-# INLINE byteArray# #-}

instance Pokeable (MutableByteArray# s) where
    type PS (MutableByteArray# s) = s

    prim :: forall a s'. Prim' a => a -> Poke s' (MutableByteArray# s')
    prim a = Poke $ \base# os# s# ->
        case writeWord8ByteArrayAs# base# os# a s# of
          s'# -> (# s'#, os# +# sizeOf# (undefined :: a) #)
    {-# INLINE prim #-}

    -- we need to touch here, but it only goes polymorphic in GHC 9.8.
    -- primitive seems to do this, so inoritaimu haha
    -- im very scared lol TODO
    byteString (BS.BS (ForeignPtr p# c) (I# len#)) = Poke $ \base# os# s# ->
        keepAlive# c (unsafeCoerce# s#) $ \s'# ->
            case copyAddrToByteArray# p# base# os# len# (unsafeCoerce# s'#) of
              s''# -> (# s''#, os# +# len# #)
    {-# INLINE byteString #-}

{- alternate implementation using touch# instead of keepalive#
        case copyAddrToByteArray# p# base# os# len# s# of
          s'# ->
            case touch# c (unsafeCoerce# s'#) of
              s''# -> (# unsafeCoerce# s''#, os# +# len# #)
-}

    -- copyByteArray# must not take the same array in different states.
    -- the runner handles the destination for us so this is fairly impossible
    byteArray# ba# (I# baos#) = Poke $ \base# os# s# ->
        let len# = sizeofByteArray# ba#
        in  case copyByteArray# ba# baos# base# os# len# s# of
              s'# -> (# s'#, os# +# len# #)
    {-# INLINE byteArray# #-}

{- Here's a helper for boxed instances:

viaAddr# :: Pokeable s Addr# => (a -> Poke s Addr#) -> a -> Poke s (Ptr Word8)
viaAddr# pokeA a = Poke $ \(Ptr base#) os# s# -> (unPoke (pokeA a)) base# os# s#

instance Pokeable RealWorld (Ptr Word8) where
    {-# INLINE w8 #-}
    w8 = viaAddr# w8

We could use Contravariant.contramap, but it requires boxed values.

-}
