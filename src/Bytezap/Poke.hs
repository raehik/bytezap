{-# LANGUAGE UnboxedTuples #-}

-- may as well export everything the interface is highly unsafe
module Bytezap.Poke where

import GHC.Exts

import GHC.IO
import Data.Word

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Data.ByteString.Short qualified as SBS
import GHC.ST ( ST(ST), runST )
import Data.Array.Byte ( MutableByteArray(..), ByteArray(..) )

type Poke# s (a :: TYPE rr) = a -> Int# -> State# s -> (# State# s, Int# #)
newtype Poke s (a :: TYPE rr) = Poke { unPoke :: Poke# s a }

-- | Sequence two 'Poke's left-to-right.
instance Semigroup (Poke s Addr#) where
    {-# INLINE (<>) #-}
    Poke l <> Poke r = Poke $ \base# os# s# ->
        case l base# os# s# of (# s'#, os'# #) -> r base# os'# s'#

-- | Sequence two 'Poke's left-to-right.
instance Semigroup (Poke s (MutableByteArray# s)) where
    {-# INLINE (<>) #-}
    Poke l <> Poke r = Poke $ \base# os# s# ->
        case l base# os# s# of (# s'#, os'# #) -> r base# os'# s'#

instance Monoid (Poke s Addr#) where
    {-# INLINE mempty #-}
    mempty = Poke $ \_base# os# s# -> (# s#, os# #)

instance Monoid (Poke s (MutableByteArray# s)) where
    {-# INLINE mempty #-}
    mempty = Poke $ \_base# os# s# -> (# s#, os# #)

-- | Execute a 'Poke' at a fresh 'BS.ByteString' of the given length.
unsafeRunPokeBS :: Int -> Poke RealWorld Addr# -> BS.ByteString
unsafeRunPokeBS len = BS.unsafeCreate len . wrapIO
{-# INLINE unsafeRunPokeBS #-}

-- TODO check if below generates same core
--wrapIO f p = void (wrapIOUptoN f p)
wrapIO :: Poke RealWorld Addr# -> Ptr Word8 -> IO ()
wrapIO (Poke p) (Ptr addr#) = IO $ \s# ->
    case p addr# 0# s# of (# s'#, _len# #) -> (# s'#, () #)
{-# INLINE wrapIO #-}

unsafeRunPokeBSUptoN :: Int -> Poke RealWorld Addr# -> BS.ByteString
unsafeRunPokeBSUptoN len = BS.unsafeCreateUptoN len . wrapIOUptoN
{-# INLINE unsafeRunPokeBSUptoN #-}

wrapIOUptoN :: Poke RealWorld Addr# -> Ptr Word8 -> IO Int
wrapIOUptoN (Poke p) (Ptr addr#) = IO $ \s# ->
    case p addr# 0# s# of (# s'#, len# #) -> (# s'#, I# len# #)
{-# INLINE wrapIOUptoN #-}

-- | Execute a 'Poke' at a fresh 'SBS.ShortByteString' of the given length.
unsafeRunPokeSBS :: Int -> (forall s. Poke s (MutableByteArray# s)) -> SBS.ShortByteString
unsafeRunPokeSBS len p = sbsUnsafeCreate len (wrapST p)
{-# INLINE unsafeRunPokeSBS #-}

-- TODO they don't export this >:( also removed the >=0 len assert
sbsUnsafeCreate
    :: Int -> (forall s. MutableByteArray s -> ST s ()) -> SBS.ShortByteString
sbsUnsafeCreate len fill = runST $ do
    mba <- unsafeNewByteArray len
    fill mba
    SBS.ShortByteString <$> unsafeFreezeByteArray mba
{-# INLINE sbsUnsafeCreate #-}

-- TODO this neither
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray mba#) = ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s', ba# #) -> (# s', ByteArray ba# #)
{-# INLINE unsafeFreezeByteArray #-}

-- TODO aaaand this neither
unsafeNewByteArray :: Int -> ST s (MutableByteArray s)
unsafeNewByteArray _len@(I# len#) = ST $ \s ->
    case newByteArray# len# s of
      (# s', mba# #) -> (# s', MutableByteArray mba# #)
{-# INLINE unsafeNewByteArray #-}

wrapST :: Poke s (MutableByteArray# s) -> MutableByteArray s -> ST s ()
wrapST (Poke p) (MutableByteArray mba#) = ST $ \s# ->
    case p mba# 0# s# of (# s'#, _len# #) -> (# s'#, () #)
{-# INLINE wrapST #-}
