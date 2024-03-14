-- | Endian shtuff
module Bytezap.Poke.Derived.Endian where

import Bytezap.Pokeable ( Pokeable(PS, prim) )
import Bytezap.Poke

import Data.Word
import Data.Int
import Raehik.Compat.Data.Int.ByteSwap
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
