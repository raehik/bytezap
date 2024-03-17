{-# LANGUAGE CPP #-} -- for a bytestring version gate >:(
{-# LANGUAGE UnboxedTuples #-}

-- may as well export everything the interface is highly unsafe
module Bytezap.Poke where

import GHC.Exts
import Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops

import GHC.IO
import GHC.Word
import GHC.Int

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Control.Monad ( void )

import Raehik.Compat.Data.Primitive.Types

import GHC.ForeignPtr

import Control.Monad.Primitive

type Poke# s = Addr# -> Int# -> State# s -> (# State# s, Int# #)

-- | Poke newtype wrapper.
newtype Poke s = Poke { unPoke :: Poke# s }

-- | Sequence two 'Poke's left-to-right.
instance Semigroup (Poke s) where
    Poke l <> Poke r = Poke $ \addr# os0# s0 ->
        case l addr# os0# s0 of (# s1, os1# #) -> r addr# os1# s1

instance Monoid (Poke s) where
    mempty = Poke $ \_addr# os# s -> (# s, os# #)

-- | Execute a 'Poke' at a fresh 'BS.ByteString' of the given length.
unsafeRunPokeBS :: Int -> Poke RealWorld -> BS.ByteString
unsafeRunPokeBS len p = BS.unsafeCreate len (void <$> unsafeRunPoke p)

-- | Execute a 'Poke' at a fresh 'BS.ByteString' of the given maximum length.
--   Does not reallocate if final size is less than estimated.
unsafeRunPokeBSUptoN :: Int -> Poke RealWorld -> BS.ByteString
unsafeRunPokeBSUptoN len = BS.unsafeCreateUptoN len . unsafeRunPoke

-- | Execute a 'Poke' at a pointer. Returns the number of bytes written.
--
-- The pointer must be a mutable buffer with enough space to hold the poke.
-- Absolutely none of this is checked. Use with caution. Sensible uses:
--
-- * implementing pokes to ByteStrings and the like
-- * executing known-length (!!) pokes to known-length (!!) buffers e.g.
--   together with allocaBytes
unsafeRunPoke :: MonadPrim s m => Poke s -> Ptr Word8 -> m Int
unsafeRunPoke (Poke p) (Ptr addr#) = primitive $ \s0 ->
    case p addr# 0# s0 of (# s1, os# #) -> (# s1, I# os# #)

-- | Poke a type via its 'Prim'' instance.
prim :: forall a s. Prim' a => a -> Poke s
prim a = Poke $ \addr# os# s0 ->
    case writeWord8OffAddrAs# addr# os# a s0 of
      s1 -> (# s1, os# +# sizeOf# (undefined :: a) #)

-- we reimplement withForeignPtr because it's too high level.
-- keepAlive# has the wrong type before GHC 9.10, but it doesn't matter here
-- because copyAddrToAddrNonOverlapping# forces RealWorld.
byteString :: BS.ByteString -> Poke RealWorld
byteString (BS.BS (ForeignPtr p# r) (I# len#)) = Poke $ \addr# os# s0 ->
    keepAlive# r s0 $ \s1 ->
        case copyAddrToAddrNonOverlapping# p# (addr# `plusAddr#` os#) len# s1 of
          s2 -> (# s2, os# +# len# #)

byteArray# :: ByteArray# -> Int# -> Int# -> Poke s
byteArray# ba# baos# balen# = Poke $ \addr# os# s0 ->
    case copyByteArrayToAddr# ba# baos# (addr# `plusAddr#` os#) balen# s0 of
      s1 -> (# s1, os# +# balen# #)

-- | essentially memset
replicateByte :: Int -> Word8 -> Poke RealWorld
replicateByte (I# len#) (W8# byte#) = Poke $ \addr# os# s0 ->
    case setAddrRange# (addr# `plusAddr#` os#) len# byteAsInt# s0 of
      s1 -> (# s1, os# +# len# #)
  where
    byteAsInt# = word2Int# (word8ToWord# byte#)
