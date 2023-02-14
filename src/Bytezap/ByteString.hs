{-# LANGUAGE UnboxedTuples #-}

module Bytezap.ByteString where

import Bytezap

import GHC.Exts
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import GHC.IO
import Data.Word
import Foreign.ForeignPtr

byteString :: B.ByteString -> Write
byteString (B.BS fptr len) = Write len (pokeForeignPtr fptr len)
{-# INLINE byteString #-}

pokeForeignPtr :: ForeignPtr Word8 -> Int -> Poke
pokeForeignPtr fptr len@(I# len#) = poke $ \addr# st# ->
    case unIO (memcpyForeignPtr (Ptr addr#) fptr len) st# of
      (# st'#, () #) -> (# st'#, addr# `plusAddr#` len# #)
{-# INLINE pokeForeignPtr #-}

memcpyForeignPtr :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyForeignPtr ptrTo fptrFrom len =
    B.unsafeWithForeignPtr fptrFrom $ \ptrFrom -> B.memcpy ptrTo ptrFrom len
{-# INLINE memcpyForeignPtr #-}
