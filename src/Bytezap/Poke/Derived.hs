{-# LANGUAGE CPP #-}

-- for WORDS_BIGENDIAN (I think)
#include "MachDeps.h"

module Bytezap.Poke.Derived where

import Bytezap.Pokeable
import Bytezap.Poke

import Data.ByteString.Short qualified as SBS
import Data.Text.Internal qualified as T
--import Data.Array.Byte qualified as A
-- ^ text-2.1 and above
import Data.Text.Array qualified as A
import Data.Word
import Data.Int
import Raehik.Compat.Data.Int.ByteSwap
import Data.Char ( ord )
import Data.Bits ( shiftR, (.&.) )
import GHC.Exts ( TYPE )

{-# INLINE w16le #-}
{-# INLINE w16be #-}
w16le, w16be :: Pokeable (ptr :: TYPE rr) => Word16 -> Poke (PS ptr) ptr
#ifdef WORDS_BIGENDIAN
w16le = prim . byteSwap16
w16be = prim
#else
w16le = prim
w16be = prim . byteSwap16
#endif

{-# INLINE w32le #-}
{-# INLINE w32be #-}
w32le, w32be :: Pokeable (ptr :: TYPE rr) => Word32 -> Poke (PS ptr) ptr
#ifdef WORDS_BIGENDIAN
w32le = prim . byteSwap32
w32be = prim
#else
w32le = prim
w32be = prim . byteSwap32
#endif

{-# INLINE w64le #-}
{-# INLINE w64be #-}
w64le, w64be :: Pokeable (ptr :: TYPE rr) => Word64 -> Poke (PS ptr) ptr
#ifdef WORDS_BIGENDIAN
w64le = prim . byteSwap64
w64be = prim
#else
w64le = prim
w64be = prim . byteSwap64
#endif

{-# INLINE i16le #-}
{-# INLINE i16be #-}
i16le, i16be :: Pokeable (ptr :: TYPE rr) => Int16 -> Poke (PS ptr) ptr
#ifdef WORDS_BIGENDIAN
i16le = prim . byteSwapI16
i16be = prim
#else
i16le = prim
i16be = prim . byteSwapI16
#endif

{-# INLINE i32le #-}
{-# INLINE i32be #-}
i32le, i32be :: Pokeable (ptr :: TYPE rr) => Int32 -> Poke (PS ptr) ptr
#ifdef WORDS_BIGENDIAN
i32le = prim . byteSwapI32
i32be = prim
#else
i32le = prim
i32be = prim . byteSwapI32
#endif

{-# INLINE i64le #-}
{-# INLINE i64be #-}
i64le, i64be :: Pokeable (ptr :: TYPE rr) => Int64 -> Poke (PS ptr) ptr
#ifdef WORDS_BIGENDIAN
i64le = prim . byteSwapI64
i64be = prim
#else
i64le = prim
i64be = prim . byteSwapI64
#endif

-- | Poke a 'SBS.ShortByteString'.
shortByteString
    :: Pokeable (ptr :: TYPE rr) => SBS.ShortByteString -> Poke (PS ptr) ptr
shortByteString (SBS.SBS ba#) = byteArray# ba# 0
{-# INLINE shortByteString #-}

-- | Poke a 'T.Text'.
text :: Pokeable (ptr :: TYPE rr) => T.Text -> Poke (PS ptr) ptr
text (T.Text (A.ByteArray ba#) os _len) = byteArray# ba# os
{-# INLINE text #-}

-- | Poke a 'Char'.
--
-- Adapted from utf8-string.
char :: Pokeable (ptr :: TYPE rr) => Char -> Poke (PS ptr) ptr
char = go . ord
 where
  w8 :: Pokeable (ptr :: TYPE rr) => Word8 -> Poke (PS ptr) ptr
  w8 = prim
  go :: Pokeable (ptr :: TYPE rr) => Int -> Poke (PS ptr) ptr
  go oc
   | oc <= 0x7f       = w8 $ fromIntegral oc

   | oc <= 0x7ff      =    w8 (fromIntegral (0xc0 + (oc `shiftR` 6)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))

   | oc <= 0xffff     =    w8 (fromIntegral (0xe0 + (oc `shiftR` 12)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 6) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))
   | otherwise        =    w8 (fromIntegral (0xf0 + (oc `shiftR` 18)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 12) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 6) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))
{-# INLINE char #-}

-- | @unsafePokeIndexed pokeAt off n@ performs @n@ indexed pokes starting from
--   @off@.
--
-- Does not check bounds. Largely intended for bytewise pokes where some work
-- needs to be performed for each byte (e.g. escaping text and poking inline).
unsafePokeIndexed
    :: Monoid (Poke s (ptr :: TYPE rr))
    => (Int -> Poke s ptr) -> Int -> Int -> Poke s ptr
-- no tail recursive option since it'd require binding a ptr, which we can't do
-- due to TYPE rr.
-- if you simply expand the monoidal ops here, you get the tail call ver. but
-- like, will GHC be able to do that?
-- hoping INLINE is good enough. but maybe need to check strictness.
unsafePokeIndexed pokeAt off n = go off
  where
    go i | i >= iend = mempty
         | otherwise = pokeAt i <> go (i+1)
    iend = off + n
{-# INLINE unsafePokeIndexed #-}
