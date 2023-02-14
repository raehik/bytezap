{-# LANGUAGE UnboxedTuples #-}

module Bytezap.Prim.ByteString where

import Bytezap.Prim

import GHC.Exts
import GHC.IO
import Data.Word
import Foreign.ForeignPtr

foreignPtr
    :: ForeignPtr Word8 -> Int -> Poke#
foreignPtr fptr len@(I# len#) addr# st# =
    case unIO (memcpyForeignPtr (Ptr addr#) fptr len) st# of
      (# st'#, () #) -> (# st'#, addr# `plusAddr#` len# #)
{-# INLINE foreignPtr #-}

memcpyForeignPtr :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyForeignPtr ptrTo fptrFrom len =
    B.unsafeWithForeignPtr fptrFrom $ \ptrFrom -> B.memcpy ptrTo ptrFrom len
{-# INLINE memcpyForeignPtr #-}
