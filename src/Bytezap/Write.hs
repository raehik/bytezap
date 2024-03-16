{-# LANGUAGE RecordWildCards #-}

-- safe module, only export the safe bits (no @Write(..)@!!)
module Bytezap.Write
  ( Write(size, poke)
  , runWriteBS, runWriteBSUptoN
  , prim, byteString, byteArray#, replicateByte
  ) where

import Bytezap.Write.Internal

import Bytezap.Poke qualified as P
import Raehik.Compat.Data.Primitive.Types
import GHC.Exts
import Data.ByteString qualified as BS
import Data.Word ( Word8 )

runWriteBS :: Write RealWorld -> BS.ByteString
runWriteBS = runWriteWith P.unsafeRunPokeBS

runWriteBSUptoN :: Write RealWorld -> BS.ByteString
runWriteBSUptoN = runWriteWith P.unsafeRunPokeBSUptoN

-- | Helper for writing 'Write' runners.
runWriteWith :: forall a s. (Int -> P.Poke s -> a) -> Write s -> a
runWriteWith runPoke (Write size poke) = runPoke size poke

prim :: forall a s. Prim' a => a -> Write s
prim a = Write (sizeOf (undefined :: a)) (P.prim a)

byteString :: BS.ByteString -> Write RealWorld
byteString bs = Write (BS.length bs) (P.byteString bs)

byteArray# :: ByteArray# -> Int# -> Int# -> Write s
byteArray# ba# baos# balen# = Write{..}
  where
    size = I# balen#
    poke = P.byteArray# ba# baos# balen#

-- | essentially memset
replicateByte :: Int -> Word8 -> Write RealWorld
replicateByte len byte = Write len (P.replicateByte len byte)
