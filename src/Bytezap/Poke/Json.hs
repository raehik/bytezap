module Bytezap.Poke.Json where

import Data.Word
import Data.Text.Internal qualified as T
import Data.Text.Array qualified as A

import Bytezap.Poke.Derived ( unsafePokeIndexed )
import Bytezap.Poke
import Bytezap.Pokeable

import GHC.Exts
import Foreign.C.Types ( CChar )
import GHC.Word

escapedLength8 :: T.Text -> Int
escapedLength8 (T.Text arr off len) = go off 0
  where
    go !i escLen
      | i >= iend = escLen
      | otherwise = go (i+1) (i + escapeW8 (A.unsafeIndex arr i))
    iend = off + len

-- my benchmarks go much faster without INLINE :/
escapeW8 :: Word8 -> Int
escapeW8 w | w >= 0x80 = 1 -- all multibyte chars are unescaped
           | otherwise = case w of 0x5C -> 2 -- \
                                   0x22 -> 2 -- "
                                   0x0A -> 2 -- \n
                                   0x0D -> 2 -- \r
                                   0x09 -> 2 -- \t
                                   _    -> if w < 0x20 then 6 else 1

pokeEscapedTextUnquoted :: Pokeable s (ptr :: TYPE rr) => T.Text -> Poke s ptr
pokeEscapedTextUnquoted (T.Text arr off len) =
    unsafePokeIndexed (pokeEscapeW8 . A.unsafeIndex arr) off len

-- think we have to poke bytes here still thanks to endianness >:(
pokeEscapeW8 :: Pokeable s (ptr :: TYPE rr) => Word8 -> Poke s ptr
pokeEscapeW8 w
  | w >= 0x80 = w8 w -- all multibyte chars are unescaped
  | otherwise =
        case w of
          0x5C -> w8 0x5C <> w8 0x5C -- \
          0x22 -> w8 0x5C <> w8 0x22 -- "
          0x0A -> w8 0x5C <> w8 0x6E -- \n
          0x0D -> w8 0x5C <> w8 0x72 -- \r
          0x09 -> w8 0x5C <> w8 0x74 -- \t
          _    ->
            if w >= 0x20 then w8 w else w8 0x5C <> w8 0x75 <> w8 0x30 <> w8 0x30 <> w8AsciiHex w

-- note I index because I _think_ this is immutable mem! but I might be wrong
-- and very stupid!!
{-# INLINE w8AsciiHex #-}
w8AsciiHex :: Pokeable s (ptr :: TYPE rr) => Word8 -> Poke s ptr
w8AsciiHex w = w16 (W16# (indexWord16OffAddr# c_lower_hex_table# wI))
  where
    !(I# wI) = fromIntegral w
    !(Ptr c_lower_hex_table#) = c_lower_hex_table

-- TODO lol I'm saying safe b/c I think it's immutable mem. but I might be
-- making a huge fucky wucky here xd
-- can't do Addr# here, no unlifted top-level values x)
foreign import ccall safe "&hs_bytestring_lower_hex_table"
    c_lower_hex_table :: Ptr CChar
