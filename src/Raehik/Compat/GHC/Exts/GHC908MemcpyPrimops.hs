{-# LANGUAGE UnboxedTuples #-}

module Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops where

import GHC.Exts
import GHC.IO ( unIO )
import Foreign.Marshal.Utils ( copyBytes, fillBytes )

copyAddrToAddrNonOverlapping#
    :: Addr# -> Addr# -> Int# -> State# RealWorld -> State# RealWorld
copyAddrToAddrNonOverlapping# src# dest# len# s# =
    case unIO (copyBytes (Ptr dest#) (Ptr src#) (fromIntegral (I# len#))) s# of
      (# s'#, () #) -> s'#

setAddrRange#
    :: Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
setAddrRange# dest# w# len# s0 =
    case unIO (fillBytes (Ptr dest#) (fromIntegral (I# w#)) (I# len#)) s0 of
      (# s1, () #) -> s1
