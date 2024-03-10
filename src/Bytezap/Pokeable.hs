{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

-- this gets us our word size and maybe endianness CPP macros
#include "MachDeps.h"

module Bytezap.Pokeable where

import Bytezap.Poke
import GHC.Exts
import Raehik.Compat.GHC.Exts.GHC910UnalignedPrimops
import Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops
import GHC.Word
import GHC.Int

import Data.ByteString.Internal qualified as BS
import GHC.ForeignPtr ( ForeignPtr(..) )

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
class Monoid (Poke s ptr) => Pokeable s (ptr :: TYPE rr) where
    w8   :: Word8  -> Poke s ptr
    w16  :: Word16 -> Poke s ptr
    w32  :: Word32 -> Poke s ptr
    w64  :: Word64 -> Poke s ptr
    i8   ::  Int8  -> Poke s ptr
    i16  ::  Int16 -> Poke s ptr
    i32  ::  Int32 -> Poke s ptr
    i64  ::  Int64 -> Poke s ptr
    int  ::  Int   -> Poke s ptr
    word :: Word   -> Poke s ptr

    -- | Poke a 'BS.ByteString'.
    byteString :: BS.ByteString        -> Poke s ptr

    -- | Poke 'ByteArray#' starting from the given 'Int' offset.
    byteArray# :: ByteArray#    -> Int -> Poke s ptr

instance Pokeable RealWorld Addr# where
    w8  (W8#  i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddr#         base# os# i# s# of
          s'# -> (# s'#, os# +# 1# #)
    {-# INLINE w8 #-}

    w16 (W16# i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsWord16# base# os# i# s# of
          s'# -> (# s'#, os# +# 2# #)
    {-# INLINE w16 #-}

    w32 (W32# i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsWord32# base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE w32 #-}

    w64 (W64# i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsWord64# base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE w64 #-}

    i8  (I8#  i#) = Poke $ \base# os# s# ->
        case writeInt8OffAddr#          base# os# i# s# of
          s'# -> (# s'#, os# +# 1# #)
    {-# INLINE i8 #-}

    i16 (I16# i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsInt16#  base# os# i# s# of
          s'# -> (# s'#, os# +# 2# #)
    {-# INLINE i16 #-}

    i32 (I32# i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsInt32#  base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE i32 #-}

    i64 (I64# i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsInt64#  base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE i64 #-}

#if WORD_SIZE_IN_BITS == 64
    int  (I#  i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsInt#    base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE int #-}

    word (W#  i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsWord#   base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE word #-}
#elif WORD_SIZE_IN_BITS == 32
    int  (I#  i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsInt#    base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE int #-}

    word (W#  i#) = Poke $ \base# os# s# ->
        case writeWord8OffAddrAsWord#   base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE word #-}
#else
#error unsupported platform (not 32/64 bit)
#endif

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

instance Pokeable s (MutableByteArray# s) where
    w8  (W8#  i#) = Poke $ \base# os# s# ->
        case writeWord8Array#           base# os# i# s# of
          s'# -> (# s'#, os# +# 1# #)
    {-# INLINE w8 #-}

    w16 (W16# i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsWord16#   base# os# i# s# of
          s'# -> (# s'#, os# +# 2# #)
    {-# INLINE w16 #-}

    w32 (W32# i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsWord32#   base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE w32 #-}

    w64 (W64# i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsWord64#   base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE w64 #-}

    i8  (I8#  i#) = Poke $ \base# os# s# ->
        case writeInt8Array#            base# os# i# s# of
          s'# -> (# s'#, os# +# 1# #)
    {-# INLINE i8 #-}

    i16 (I16# i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsInt16#    base# os# i# s# of
          s'# -> (# s'#, os# +# 2# #)
    {-# INLINE i16 #-}

    i32 (I32# i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsInt32#    base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE i32 #-}

    i64 (I64# i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsInt64#    base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE i64 #-}

#if WORD_SIZE_IN_BITS == 64
    int  (I#  i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsInt#      base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE int #-}

    word (W#  i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsWord#     base# os# i# s# of
          s'# -> (# s'#, os# +# 8# #)
    {-# INLINE word #-}
#elif WORD_SIZE_IN_BITS == 32
    int  (I#  i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsInt#      base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE int #-}

    word (W#  i#) = Poke $ \base# os# s# ->
        case writeWord8ArrayAsWord#     base# os# i# s# of
          s'# -> (# s'#, os# +# 4# #)
    {-# INLINE word #-}
#else
#error unsupported platform (not 32/64 bit)
#endif

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
