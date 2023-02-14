{-# LANGUAGE UnboxedTuples #-}

module Bytezap.Integer where

import Bytezap
import Bytezap.Prim.Integer qualified as Prim
import GHC.Exts
import Data.Word

-- | Construct a 'Write' of the following length using the given primitive poke.
writeViaPrim
    :: Int# -> (forall s. Addr# -> a -> State# s -> State# s) -> a -> Write
writeViaPrim len# prim a = write (I# len#) $ \addr# st# ->
    case prim addr# a st# of
      st'# -> (# st'#, addr# `plusAddr#` len# #)
{-# INLINE writeViaPrim #-}

w8 :: Word8 -> Write
w8 = writeViaPrim 1# Prim.u8#
{-# INLINE w8 #-}

-- TODO check core equality
w8' :: Word8 -> Write
w8' a = write 1 $ \addr# st# ->
    case Prim.u8# addr# a st# of
      st'# -> (# st'#, addr# `plusAddr#` 1# #)
{-# INLINE w8' #-}

w16le, w16be :: Word16 -> Write
w16le = writeViaPrim 2# Prim.u16le#
w16be = writeViaPrim 2# Prim.u16be#
{-# INLINE w16le #-}
{-# INLINE w16be #-}

w32le, w32be :: Word32 -> Write
w32le = writeViaPrim 4# Prim.u32le#
w32be = writeViaPrim 4# Prim.u32be#
{-# INLINE w32le #-}
{-# INLINE w32be #-}

w64le, w64be :: Word64 -> Write
w64le = writeViaPrim 8# Prim.u64le#
w64be = writeViaPrim 8# Prim.u64be#
{-# INLINE w64le #-}
{-# INLINE w64be #-}
