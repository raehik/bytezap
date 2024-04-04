{-# LANGUAGE UnboxedTuples #-}

{- | Struct serializer: serialize fields of known length.

In Haskell-ish terminology, one may consider a C struct to be a product type
where each field is of known length. Thus, fields may be accessed by a fixed
offset from the struct start. This is convenient for efficient access, since
those offsets may be turned into immediates on a register in a MOV instruction.

Given a struct-like type, we don't need to track "bytes serialized so far" like
the general case. We can serialize fields in any order we like, since we know
where they will sit in the resulting bytestring.

This module provides a serializer specifically for these struct-like types.
Maybe GHC can write more efficient code for these super-simple types!
I have no idea. So I'm trying it, and will compare performance.

Notably, this serializer is much less flexible. No monoid! I don't really expect
anyone to write manual stuff with it-- you should just use the generics.
That reminds me, TODO could easily provide some TH too, and again compare.
-}

module Bytezap.Struct where

import GHC.Exts
import Raehik.Compat.Data.Primitive.Types

import Control.Monad.Primitive ( MonadPrim, primitive )
import Data.Word ( Word8 )
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

-- | A struct poker: base address (constant), byte offset, state token.
--
-- We could combine base address and byte offset, but we're aiming for code that
-- stores the address in a register and uses immediates to access fields (like
-- a good C compiler will do for its structs). So by keeping them separate, I'm
-- hoping that we can nudge GHC towards such behaviour.
type Poke# s = Addr# -> Int# -> State# s -> State# s

-- | Poke newtype wrapper.
newtype Poke s = Poke { unPoke :: Poke# s }

-- One may write a valid 'Semigroup' instance, but it's nonsensical, so let's
-- not.

-- | Execute a 'Poke' at a fresh 'BS.ByteString' of the given length.
unsafeRunPokeBS :: Int -> Poke RealWorld -> BS.ByteString
unsafeRunPokeBS len p = BS.unsafeCreate len (unsafeRunPoke p)

-- | Execute a 'Poke' at a pointer. Returns the number of bytes written.
--
-- The pointer must be a mutable buffer with enough space to hold the poke.
-- Absolutely none of this is checked. Use with caution. Sensible uses:
--
-- * implementing pokes to ByteStrings and the like
-- * executing known-length (!!) pokes to known-length (!!) buffers e.g.
--   together with allocaBytes
unsafeRunPoke :: MonadPrim s m => Poke s -> Ptr Word8 -> m ()
unsafeRunPoke (Poke p) (Ptr base#) = primitive $ \s0 -> (# p base# 0# s0, () #)

-- | Poke a type via its 'Prim'' instance.
prim :: forall a s. Prim' a => a -> Poke s
prim a = Poke $ \base# os# s0 -> writeWord8OffAddrAs# base# os# a s0
