{-# LANGUAGE CPP #-} -- for a bytestring version gate >:(
{-# LANGUAGE UnboxedTuples #-}

-- may as well export everything the interface is highly unsafe
module Bytezap.Poke where

import GHC.Exts
import Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops

import GHC.Word ( Word8(W8#) )

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Control.Monad ( void )

import Raehik.Compat.Data.Primitive.Types

import GHC.ForeignPtr

import Control.Monad.Primitive

import Bytezap.Struct qualified as Struct

type Poke# s = Addr# -> Int# -> State# s -> (# State# s, Int# #)

-- | Poke newtype wrapper.
newtype Poke s = Poke { unPoke :: Poke# s }

-- | Sequence two 'Poke's left-to-right.
instance Semigroup (Poke s) where
    Poke l <> Poke r = Poke $ \base# os0# s0 ->
        case l base# os0# s0 of (# s1, os1# #) -> r base# os1# s1

instance Monoid (Poke s) where
    mempty = Poke $ \_base# os# s -> (# s, os# #)

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
unsafeRunPoke (Poke p) (Ptr base#) = primitive $ \s0 ->
    case p base# 0# s0 of (# s1, os# #) -> (# s1, I# os# #)

-- | Poke a type via its 'Prim'' instance.
prim :: forall a s. Prim' a => a -> Poke s
prim a = Poke $ \base# os# s0 ->
    case writeWord8OffAddrAs# base# os# a s0 of
      s1 -> (# s1, os# +# sizeOf# (undefined :: a) #)

-- we reimplement withForeignPtr because it's too high level.
-- keepAlive# has the wrong type before GHC 9.10, but it doesn't matter here
-- because copyAddrToAddrNonOverlapping# forces RealWorld.
byteString :: BS.ByteString -> Poke RealWorld
byteString (BS.BS (ForeignPtr p# r) (I# len#)) = Poke $ \base# os# s0 ->
    keepAlive# r s0 $ \s1 ->
        case copyAddrToAddrNonOverlapping# p# (base# `plusAddr#` os#) len# s1 of
          s2 -> (# s2, os# +# len# #)

byteArray# :: ByteArray# -> Int# -> Int# -> Poke s
byteArray# ba# baos# balen# = Poke $ \base# os# s0 ->
    case copyByteArrayToAddr# ba# baos# (base# `plusAddr#` os#) balen# s0 of
      s1 -> (# s1, os# +# balen# #)

-- | essentially memset
replicateByte :: Int -> Word8 -> Poke RealWorld
replicateByte (I# len#) (W8# byte#) = Poke $ \base# os# s0 ->
    case setAddrRange# (base# `plusAddr#` os#) len# byteAsInt# s0 of
      s1 -> (# s1, os# +# len# #)
  where
    byteAsInt# = word2Int# (word8ToWord# byte#)

-- | Use a struct poke as a regular poke.
--
-- To do this, we must associate a constant byte length with an existing poker.
-- Note that pokers don't expose the type of the data they are serializing,
-- so this is a very clumsy operation by itself. You should only be using this
-- when you have such types in scope, and the constant length should be obtained
-- in a sensible manner (e.g. 'Bytezap.Struct.Generic.KnownSizeOf' for generic
-- struct pokers, or your own constant size class if you're doing funky stuff).
fromStructPoke :: Int -> Struct.Poke s -> Poke s
fromStructPoke (I# len#) (Struct.Poke p) = Poke $ \base# os# s ->
    (# p base# os# s, len# #)
