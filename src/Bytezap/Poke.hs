{-# LANGUAGE CPP #-} -- for a bytestring version gate >:(
{-# LANGUAGE UnboxedTuples #-}

-- may as well export everything the interface is highly unsafe
module Bytezap.Poke where

import GHC.Exts
import Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops

import GHC.IO
import Data.Word

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Control.Monad ( void )

import Raehik.Compat.Data.Primitive.Types

import GHC.ForeignPtr

type Poke# s = Addr# -> Int# -> State# s -> (# State# s, Int# #)

-- | Poke newtype wrapper.
newtype Poke s = Poke { unPoke :: Poke# s }

-- | Sequence two 'Poke's left-to-right.
instance Semigroup (Poke s) where
    Poke l <> Poke r = Poke $ \addr# os# s0 ->
        case l addr# os# s0 of (# s1, os'# #) -> r addr# os'# s1

instance Monoid (Poke s) where
    mempty = Poke $ \_addr# os# s0 -> (# s0, os# #)

-- | Execute a 'Poke' at a fresh 'BS.ByteString' of the given length.
unsafeRunPokeBS :: Int -> Poke RealWorld -> BS.ByteString
unsafeRunPokeBS len = BS.unsafeCreate len . wrapIO

wrapIO :: Poke RealWorld -> Ptr Word8 -> IO ()
wrapIO f p = void (wrapIOUptoN f p)

wrapIOUptoN :: Poke RealWorld -> Ptr Word8 -> IO Int
wrapIOUptoN (Poke p) (Ptr addr#) = IO $ \s0 ->
    case p addr# 0# s0 of (# s1, len# #) -> (# s1, I# len# #)

-- | Execute a 'Poke' at a fresh 'BS.ByteString' of the given maximum length.
--   Does not reallocate if final size is less than estimated.
unsafeRunPokeBSUptoN :: Int -> Poke RealWorld -> BS.ByteString
unsafeRunPokeBSUptoN len = BS.unsafeCreateUptoN len . wrapIOUptoN

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
