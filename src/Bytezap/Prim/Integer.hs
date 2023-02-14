{-# LANGUAGE CPP #-}

module Bytezap.Prim.Integer
  (
  -- * Native endianness
    u8#
  , u16Host#
  , u32Host#
  , u64Host#

  -- * Little endian
  , u16le#
  , u32le#
  , u64le#

  -- * Big endian
  , u16be#
  , u32be#
  , u64be#

  -- * Reverse host endianness
  , u16Rev#
  , u32Rev#
  , u64Rev#
  ) where

import GHC.Exts
import Data.Word
import GHC.Word

{-# INLINE u8# #-}
u8# :: Addr# -> Word8 -> State#s -> State# s
u8# addr# (W8# a) st = writeWord8OffAddr# addr# 0# a st

{-# INLINE u16Host# #-}
u16Host# :: Addr# -> Word16 -> State# s -> State# s
u16Host# addr# (W16# a) st = writeWord16OffAddr# addr# 0# a st

{-# INLINE u32Host# #-}
u32Host# :: Addr# -> Word32 -> State# s -> State# s
u32Host# addr# (W32# a) st = writeWord32OffAddr# addr# 0# a st

{-# INLINE u64Host# #-}
u64Host# :: Addr# -> Word64 -> State# s -> State# s
u64Host# addr# (W64# a) st = writeWord64OffAddr# addr# 0# a st

{-# INLINE u16Rev# #-}
u16Rev# :: Addr# -> Word16 -> State# s -> State# s
u16Rev# addr# a = u16Host# addr# (byteSwap16 a)

{-# INLINE u32Rev# #-}
u32Rev# :: Addr# -> Word32 -> State# s -> State# s
u32Rev# addr# a = u32Host# addr# (byteSwap32 a)

{-# INLINE u64Rev# #-}
u64Rev# :: Addr# -> Word64 -> State# s -> State# s
u64Rev# addr# a = u64Host# addr# (byteSwap64 a)

{-# INLINE u16le# #-}
{-# INLINE u16be# #-}
u16le#, u16be# :: Addr# -> Word16 -> State# s -> State# s
u16le# =
#ifdef WORDS_BIGENDIAN
    u16Rev#
#else
    u16Host#
#endif
u16be# =
#ifdef WORDS_BIGENDIAN
    u16Host#
#else
    u16Rev#
#endif

{-# INLINE u32le# #-}
{-# INLINE u32be# #-}
u32le#, u32be# :: Addr# -> Word32 -> State# s -> State# s
u32le# =
#ifdef WORDS_BIGENDIAN
    u32Rev#
#else
    u32Host#
#endif
u32be# =
#ifdef WORDS_BIGENDIAN
    u32Host#
#else
    u32Rev#
#endif

{-# INLINE u64le# #-}
{-# INLINE u64be# #-}
u64le#, u64be# :: Addr# -> Word64 -> State# s -> State# s
u64le# =
#ifdef WORDS_BIGENDIAN
    u64Rev#
#else
    u64Host#
#endif
u64be# =
#ifdef WORDS_BIGENDIAN
    u64Host#
#else
    u64Rev#
#endif
