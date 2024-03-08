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

runPokeBS :: Int -> Poke RealWorld Addr# -> BS.ByteString
runPokeBS len = BS.unsafeCreate len . wrapIO
{-# INLINE runPokeBS #-}

wrapIO :: Poke RealWorld Addr# -> Ptr Word8 -> IO ()
wrapIO (Poke p) (Ptr addr#) =
    IO (\s# -> case p addr# 0# s# of (# s'#, _os'# #) -> (# s'#, () #))
{-# INLINE wrapIO #-}

-- Int >= 0 or we fking explode
runPokeSBS :: Int -> (forall s. Poke s (MutableByteArray# s)) -> SBS.ShortByteString
runPokeSBS len p = sbsUnsafeCreate len (wrapST p)
{-# INLINE runPokeSBS #-}

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
unsafeFreezeByteArray (MutableByteArray mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', ByteArray ba# #)

-- TODO aaaand this neither
unsafeNewByteArray :: Int -> ST s (MutableByteArray s)
unsafeNewByteArray len@(I# len#) = ST $ \s ->
    case newByteArray# len# s of
      (# s', mba# #) -> (# s', MutableByteArray mba# #)

wrapST :: Poke s (MutableByteArray# s) -> MutableByteArray s -> ST s ()
wrapST (Poke p) (MutableByteArray mba#) = ST $ \s# ->
    case p mba# 0# s# of (# s'#, _os# #) -> (# s'#, () #)

{-

-- | Construct a 'Poke'.
poke :: Poke# s a -> Poke s a
poke = Poke
{-# INLINE poke #-}

runPokeBSUptoN :: Int -> Poke RealWorld -> BS.ByteString
runPokeBSUptoN len = BS.unsafeCreateUptoN len . wrapIOUptoN
{-# INLINE runPokeBSUptoN #-}

wrapIOUptoN :: Poke RealWorld -> Ptr Word8 -> IO Int
wrapIOUptoN (Poke p) (Ptr addr#) = IO $ \st# ->
    case p addr# st# of
      (# st'#, addr'# #) -> (# st'#, I# (addr'# `minusAddr#` addr#) #)
{-# INLINE wrapIOUptoN #-}

-- | Allocate a buffer of the given size and run a 'Poke' over it.
--
-- The 'Poke' must fill the buffer exactly. If it goes under, you should get
-- some random garbage at the end. If it goes over, your computer will probably
-- explode.
runPoke :: Int -> Poke s -> B.ByteString
runPoke len = B.unsafeCreate len . wrapPoke
{-# INLINE runPoke #-}

wrapPoke :: Poke -> Ptr Word8 -> IO ()
wrapPoke (Poke p) (Ptr addr#) =
    IO (\st# -> case p addr# st# of (# l, _r #) -> (# l, () #))
{-# INLINE wrapPoke #-}

-- | Instructions on how to perform a sized write.
--
-- The 'Poke' in 'writePoke' must write the _exact_ number of bytes specified in
-- 'writeSize'. Otherwise, your computer explodes.
data Write = Write
  { writeSize :: {-# UNPACK #-} !Int
  , writePoke :: !Poke -- unpack unusable TODO is strict good or not here
  }

-- | Construct a 'Write'.
write :: Int -> Poke# -> Write
write len p = Write len (Poke p)
{-# INLINE write #-}

-- | Sequence the 'Poke's, sum the sizes.
instance Semigroup Write where
    -- TODO feels like this might be INLINE[1] or even INLINE[0]?
    {-# INLINE (<>) #-}
    Write ll lp <> Write rl rp = Write (ll + rl) (lp <> rp)

-- | The empty 'Write' is the empty 'Poke', which writes zero bytes.
instance Monoid Write where
    {-# INLINE mempty #-}
    mempty = Write 0 mempty

-- | Serialize and show the resulting ByteString.
instance Show Write where showsPrec p = showsPrec p . runWrite

runWrite :: Write -> B.ByteString
runWrite (Write len p) = runPoke len p
{-# INLINE runWrite #-}

-}
