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
w16le, w16be :: Pokeable s (ptr :: TYPE rr) => Word16 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
w16le = w16 . byteSwap16
w16be = w16
#else
w16le = w16
w16be = w16 . byteSwap16
#endif

{-# INLINE w32le #-}
{-# INLINE w32be #-}
w32le, w32be :: Pokeable s (ptr :: TYPE rr) => Word32 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
w32le = w32 . byteSwap32
w32be = w32
#else
w32le = w32
w32be = w32 . byteSwap32
#endif

{-# INLINE w64le #-}
{-# INLINE w64be #-}
w64le, w64be :: Pokeable s (ptr :: TYPE rr) => Word64 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
w64le = w64 . byteSwap64
w64be = w64
#else
w64le = w64
w64be = w64 . byteSwap64
#endif

{-# INLINE i16le #-}
{-# INLINE i16be #-}
i16le, i16be :: Pokeable s (ptr :: TYPE rr) => Int16 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
i16le = i16 . byteSwapI16
i16be = i16
#else
i16le = i16
i16be = i16 . byteSwapI16
#endif

{-# INLINE i32le #-}
{-# INLINE i32be #-}
i32le, i32be :: Pokeable s (ptr :: TYPE rr) => Int32 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
i32le = i32 . byteSwapI32
i32be = i32
#else
i32le = i32
i32be = i32 . byteSwapI32
#endif

{-# INLINE i64le #-}
{-# INLINE i64be #-}
i64le, i64be :: Pokeable s (ptr :: TYPE rr) => Int64 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
i64le = i64 . byteSwapI64
i64be = i64
#else
i64le = i64
i64be = i64 . byteSwapI64
#endif

-- | Poke a 'SBS.ShortByteString'.
shortByteString
    :: Pokeable s (ptr :: TYPE rr) => SBS.ShortByteString -> Poke s ptr
shortByteString (SBS.SBS ba#) = byteArray# ba# 0
{-# INLINE shortByteString #-}

-- | Poke a 'T.Text'.
text :: Pokeable s (ptr :: TYPE rr) => T.Text -> Poke s ptr
text (T.Text (A.ByteArray ba#) os _len) = byteArray# ba# os
{-# INLINE text #-}

-- | Poke a 'Char'.
--
-- Adapted from utf8-string.
char :: Pokeable s (ptr :: TYPE rr) => Char -> Poke s ptr
char = go . ord
 where
  go :: Pokeable s (ptr :: TYPE rr) => Int -> Poke s ptr
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
