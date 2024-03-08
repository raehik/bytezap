{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

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

import Data.ByteString.Short qualified as SBS
import Data.Text.Internal qualified as T
import Data.Array.Byte qualified as A

import Data.Char ( ord )
import Data.Bits ( shiftR, (.&.) )

-- | Types that support direct, low-level "poking" operations, where values are
--   pointers of some sort.
--
-- This type class enables us to write a single program which may then serialize
-- out to either pinned memory (via an @Addr#@ unwrapped from a @ByteString@),
-- or unpinned memory (the 'MutableByteArray#' inside a @ShortByteString@).
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

    byteString :: BS.ByteString        -> Poke s ptr

    -- Poke a 'ByteArray#' starting from the given 'Int' offset.
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

{-# INLINE w16le #-}
{-# INLINE w16be #-}
w16le, w16be :: Pokeable s ptr => Word16 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
w16le = w16 . byteSwap16
w16be = w16
#else
w16le = w16
w16be = w16 . byteSwap16
#endif

{-# INLINE w32le #-}
{-# INLINE w32be #-}
w32le, w32be :: Pokeable s ptr => Word32 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
w32le = w32 . byteSwap32
w32be = w32
#else
w32le = w32
w32be = w32 . byteSwap32
#endif

{-# INLINE w64le #-}
{-# INLINE w64be #-}
w64le, w64be :: Pokeable s ptr => Word64 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
w64le = w64 . byteSwap64
w64be = w64
#else
w64le = w64
w64be = w64 . byteSwap64
#endif

-- these look so silly lol

byteSwapI16 :: Int16 -> Int16
byteSwapI16 (I16# i#) = I16# (word16ToInt16# (wordToWord16# (byteSwap16# (word16ToWord# (int16ToWord16# i#)))))
{-# INLINE byteSwapI16 #-}

byteSwapI32 :: Int32 -> Int32
byteSwapI32 (I32# i#) = I32# (word32ToInt32# (wordToWord32# (byteSwap32# (word32ToWord# (int32ToWord32# i#)))))
{-# INLINE byteSwapI32 #-}

byteSwapI64 :: Int64 -> Int64
byteSwapI64 (I64# i#) = I64# (word64ToInt64# (byteSwap64# (int64ToWord64# i#)))
{-# INLINE byteSwapI64 #-}

{-# INLINE i16le #-}
{-# INLINE i16be #-}
i16le, i16be :: Pokeable s ptr => Int16 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
i16le = i16 . byteSwapI16
i16be = i16
#else
i16le = i16
i16be = i16 . byteSwapI16
#endif

{-# INLINE i32le #-}
{-# INLINE i32be #-}
i32le, i32be :: Pokeable s ptr => Int32 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
i32le = i32 . byteSwapI32
i32be = i32
#else
i32le = i32
i32be = i32 . byteSwapI32
#endif

{-# INLINE i64le #-}
{-# INLINE i64be #-}
i64le, i64be :: Pokeable s ptr => Int64 -> Poke s ptr
#ifdef WORDS_BIGENDIAN
i64le = i64 . byteSwapI64
i64be = i64
#else
i64le = i64
i64be = i64 . byteSwapI64
#endif

shortByteString :: Pokeable s ptr => SBS.ShortByteString -> Poke s ptr
shortByteString (SBS.SBS ba#) = byteArray# ba# 0

text :: Pokeable s ptr => T.Text -> Poke s ptr
text (T.Text (A.ByteArray ba#) os _len) = byteArray# ba# os

-- TODO adapted from utf8-string
charUtf8 :: Pokeable s ptr => Char -> Poke s ptr
charUtf8 = go . ord
 where
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
{-# INLINE charUtf8 #-}
