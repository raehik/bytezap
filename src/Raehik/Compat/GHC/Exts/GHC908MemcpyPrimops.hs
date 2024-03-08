{-# LANGUAGE UnboxedTuples #-}

module Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops where

import GHC.Exts
import GHC.IO ( unIO )
import Foreign.Marshal.Utils ( copyBytes )

copyAddrToAddrNonOverlapping#
    :: Addr# -> Addr# -> Int# -> State# RealWorld -> State# RealWorld
copyAddrToAddrNonOverlapping# src# dest# len# s# =
    case unIO (copyBytes (Ptr dest#) (Ptr src#) (fromIntegral (I# len#))) s# of
      (# s'#, () #) -> s'#
{-# INLINE copyAddrToAddrNonOverlapping# #-}
