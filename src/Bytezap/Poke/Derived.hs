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
import GHC.ByteOrder ( ByteOrder(BigEndian, LittleEndian), targetByteOrder )

w16le, w16be :: Pokeable (ptr :: TYPE rr) => Word16 -> Poke (PS ptr) ptr
w16le = case targetByteOrder of LittleEndian -> prim
                                BigEndian    -> prim . byteSwap16
w16be = case targetByteOrder of LittleEndian -> prim . byteSwap16
                                BigEndian    -> prim

w32le, w32be :: Pokeable (ptr :: TYPE rr) => Word32 -> Poke (PS ptr) ptr
w32le = case targetByteOrder of LittleEndian -> prim
                                BigEndian    -> prim . byteSwap32
w32be = case targetByteOrder of LittleEndian -> prim . byteSwap32
                                BigEndian    -> prim

w64le, w64be :: Pokeable (ptr :: TYPE rr) => Word64 -> Poke (PS ptr) ptr
w64le = case targetByteOrder of LittleEndian -> prim
                                BigEndian    -> prim . byteSwap64
w64be = case targetByteOrder of LittleEndian -> prim . byteSwap64
                                BigEndian    -> prim

i16le, i16be :: Pokeable (ptr :: TYPE rr) =>  Int16 -> Poke (PS ptr) ptr
i16le = case targetByteOrder of LittleEndian -> prim
                                BigEndian    -> prim . byteSwapI16
i16be = case targetByteOrder of LittleEndian -> prim . byteSwapI16
                                BigEndian    -> prim

i32le, i32be :: Pokeable (ptr :: TYPE rr) =>  Int32 -> Poke (PS ptr) ptr
i32le = case targetByteOrder of LittleEndian -> prim
                                BigEndian    -> prim . byteSwapI32
i32be = case targetByteOrder of LittleEndian -> prim . byteSwapI32
                                BigEndian    -> prim

i64le, i64be :: Pokeable (ptr :: TYPE rr) =>  Int64 -> Poke (PS ptr) ptr
i64le = case targetByteOrder of LittleEndian -> prim
                                BigEndian    -> prim . byteSwapI64
i64be = case targetByteOrder of LittleEndian -> prim . byteSwapI64
                                BigEndian    -> prim

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
