{-# LANGUAGE UnboxedTuples #-}

module Bytezap where

import GHC.Exts
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import GHC.IO
import Data.Word

-- | Unboxed poke operation.
--
-- A newtype allows us a monoidal interface.
newtype Poke = Poke
  { -- | Write at an address and return the next address.
    --
    -- The returned address must be after the argument address.
    --
    -- TODO I use that output order because it matches IO. Probs doesn't matter.
    unPoke :: Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
  }

-- | Construct a 'Poke'.
poke :: (Addr# -> State# RealWorld-> (# State# RealWorld, Addr# #)) -> Poke
poke = Poke
{-# INLINE poke #-}

-- | Sequence two 'Poke's left-to-right.
instance Semigroup Poke where
    {-# INLINE (<>) #-}
    Poke l <> Poke r = Poke $ \addr# st# ->
        case l addr# st# of (# st'#, addr'# #) -> r addr'# st'#

-- | The empty 'Poke' simply returns its arguments.
instance Monoid Poke where
    {-# INLINE mempty #-}
    mempty = Poke $ \addr# st# -> (# st#, addr# #)

-- | Allocate a buffer of the given length and run a 'Poke' over it.
--
-- The 'Poke' must fill the buffer exactly. If it goes under, you should get
-- some random garbage at the end. If it goes over, your computer will probably
-- explode.
runPoke :: Int -> Poke -> B.ByteString
runPoke len = B.unsafeCreate len . wrapPoke
{-# INLINE runPoke #-}

wrapPoke :: Poke -> Ptr Word8 -> IO ()
wrapPoke (Poke p) (Ptr addr#) =
    IO (\st# -> case p addr# st# of (# l, _r #) -> (# l, () #))
{-# INLINE wrapPoke #-}

-- | Instructions on how to perform a sized write.
--
-- The 'PokeAction' in 'writeActionPoke' must write the _exact_ number of bytes
-- specified in 'writeActionLength'. Otherwise, your computer explodes.
data Write = Write
  { writeLength :: {-# UNPACK #-} !Int
  , writePoke   :: !Poke -- unpack unusable (TODO confirm now is typesym)
  }

-- | Construct a 'Write'.
write
    :: Int
    -> (Addr# -> State# RealWorld-> (# State# RealWorld, Addr# #))
    -> Write
write len p = Write len (Poke p)
{-# INLINE write #-}

-- | Sequence the 'Poke's, sum the lengths.
instance Semigroup Write where
    {-# INLINE (<>) #-}
    Write ll lp <> Write rl rp = Write (ll + rl) (lp <> rp)

-- | The empty 'Write' is the empty 'Poke', which writes zero bytes.
instance Monoid Write where
    {-# INLINE mempty #-}
    mempty = Write 0 mempty

runWrite :: Write -> B.ByteString
runWrite (Write len p) = runPoke len p
{-# INLINE runWrite #-}
