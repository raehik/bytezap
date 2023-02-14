{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module Bytezap.Integer where

import Bytezap
--import Bytezap.Prim.Integer qualified as Prim
import GHC.Exts
import Data.Word
import GHC.Word

-- | Construct a 'Write' of the following length using the given primitive poke.
writeViaPrim
    :: Int#
    -> (forall s. Addr# -> Int# -> a -> State# s -> State# s)
    -> a -> Write
writeViaPrim len# writeOffPrim a = write (I# len#) $ \addr# os# st# ->
    case writeOffPrim addr# os# a st# of
      st'# -> (# st'#, os# +# len# #)
{-# INLINE writeViaPrim #-}

w8 :: Word8 -> Write
w8 (W8# a#) = write 1 $ \addr# os# st# ->
    case writeWord8OffAddr# addr# os# a# st# of
      st'# -> (# st'#, os# +# 1# #)
{-# INLINE w8 #-}

w16 :: Word16 -> Write
w16 (W16# a#) = write 2 $ \addr# os# st# ->
    case writeWord16OffAddr# addr# os# a# st# of
      st'# -> (# st'#, os# +# 2# #)
{-# INLINE w16 #-}

w32 :: Word32 -> Write
w32 (W32# a#) = write 4 $ \addr# os# st# ->
    case writeWord32OffAddr# addr# os# a# st# of
      st'# -> (# st'#, os# +# 4# #)
{-# INLINE w32 #-}

w64 :: Word64 -> Write
w64 (W64# a#) = write 8 $ \addr# os# st# ->
    case writeWord64OffAddr# addr# os# a# st# of
      st'# -> (# st'#, os# +# 8# #)
{-# INLINE w64 #-}

{-# INLINE w16le #-}
{-# INLINE w16be #-}
w16le, w16be :: Word16 -> Write
#ifdef WORDS_BIGENDIAN
w16le = w16 . byteSwap16
w16be = w16
#else
w16le = w16
w16be = w16 . byteSwap16
#endif

{-# INLINE w32le #-}
{-# INLINE w32be #-}
w32le, w32be :: Word32 -> Write
#ifdef WORDS_BIGENDIAN
w32le = w32 . byteSwap32
w32be = w32
#else
w32le = w32
w32be = w32 . byteSwap32
#endif

{-# INLINE w64le #-}
{-# INLINE w64be #-}
w64le, w64be :: Word64 -> Write
#ifdef WORDS_BIGENDIAN
w64le = w64 . byteSwap64
w64be = w64
#else
w64le = w64
w64be = w64 . byteSwap64
#endif
