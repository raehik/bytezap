{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module Raehik.Compat.Data.Primitive.Types
  ( Prim'(..)
  , P.Prim(..)
  , P.sizeOf
  ) where

import Data.Primitive.Types qualified as P
import Raehik.Compat.GHC.Exts.GHC910UnalignedAddrPrimops
import GHC.Exts
import GHC.Word
import GHC.Int
import Numeric.Natural

-- | 'P.Prim' extension class providing unaligned accesses
--
-- hoping to get this merged in https://github.com/haskell/primitive/issues/409
--
-- (also includes Addr# primops which that issue/PR may not)
--
-- Also includes an associated type for size in bytes. Another thing that maybe
-- primitive could provide. (Wouldn't be hard!)
class P.Prim a => Prim' a where
    type SizeOf a :: Natural
    -- | Read a value from the array. The offset is in bytes.
    indexWord8ByteArrayAs# :: ByteArray# -> Int# -> a

    readWord8ByteArrayAs#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    writeWord8ByteArrayAs# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

    indexWord8OffAddrAs# :: Addr# -> Int# -> a

    readWord8OffAddrAs#  :: Addr# -> Int# -> State# s -> (# State# s, a #)
    writeWord8OffAddrAs# :: Addr# -> Int# -> a -> State# s -> State# s

instance Prim' Word8 where
    type SizeOf Word8 = SIZEOF_WORD8
    indexWord8ByteArrayAs# = P.indexByteArray#
    readWord8ByteArrayAs#  = P.readByteArray#
    writeWord8ByteArrayAs# = P.writeByteArray#
    indexWord8OffAddrAs#   = P.indexOffAddr#
    readWord8OffAddrAs#    = P.readOffAddr#
    writeWord8OffAddrAs#   = P.writeOffAddr#

instance Prim' Word16 where
    type SizeOf Word16 = SIZEOF_WORD16
    indexWord8ByteArrayAs# arr# os# = W16# (indexWord8ArrayAsWord16# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsWord16# arr# os# s0 of
          (# s1, w# #) -> (# s1, W16# w# #)
    writeWord8ByteArrayAs# arr# os# (W16# w#) = \s0 ->
        writeWord8ArrayAsWord16# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = W16# (indexWord8OffAddrAsWord16# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsWord16# addr# os# s0 of
          (# s1, w# #) -> (# s1, W16# w# #)
    writeWord8OffAddrAs# addr# os# (W16# w#) s# =
        writeWord8OffAddrAsWord16# addr# os# w# s#

instance Prim' Word32 where
    type SizeOf Word32 = SIZEOF_WORD32
    writeWord8OffAddrAs# base# os# (W32# i#) s# =
        writeWord8OffAddrAsWord32# base# os# i# s#

instance Prim' Word64 where
    type SizeOf Word64 = SIZEOF_WORD64
    writeWord8OffAddrAs# base# os# (W64# i#) s# =
        writeWord8OffAddrAsWord64# base# os# i# s#

instance Prim' Int8 where
    type SizeOf Int8 = SIZEOF_INT8
    indexWord8ByteArrayAs# = P.indexByteArray#
    readWord8ByteArrayAs#  = P.readByteArray#
    writeWord8ByteArrayAs# = P.writeByteArray#
    indexWord8OffAddrAs#   = P.indexOffAddr#
    readWord8OffAddrAs#    = P.readOffAddr#
    writeWord8OffAddrAs#   = P.writeOffAddr#


instance Prim' Int16 where
    type SizeOf Int16 = SIZEOF_INT16
    writeWord8OffAddrAs# base# os# (I16# i#) s# =
        writeWord8OffAddrAsInt16# base# os# i# s#

instance Prim' Int32 where
    type SizeOf Int32 = SIZEOF_INT32
    writeWord8OffAddrAs# base# os# (I32# i#) s# =
        writeWord8OffAddrAsInt32# base# os# i# s#

instance Prim' Int64 where
    type SizeOf Int64 = SIZEOF_INT64
    writeWord8OffAddrAs# base# os# (I64# i#) s# =
        writeWord8OffAddrAsInt64# base# os# i# s#

instance Prim' Word where
    type SizeOf Word = SIZEOF_HSWORD
    writeWord8OffAddrAs# base# os# (W# i#) s# =
        writeWord8OffAddrAsWord# base# os# i# s#

instance Prim' Int where
    type SizeOf Int = SIZEOF_HSINT
    writeWord8OffAddrAs# base# os# (I# i#) s# =
        writeWord8OffAddrAsInt# base# os# i# s#
