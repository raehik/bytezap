{- | Efficient type-level bytestring serialization.

@['Natural']@s have a convenient syntax, and we can use them as a type-level
bytestring by asserting that each 'Natural' is <=255 when reifying. This module
provides type classes which give you a serializer for a given @['Natural']@.

We maximize efficiency by grouping bytes into machine words. We have to be
pretty verbose to achieve this. Each type class attempts to group bytes into its
machine word type, and if it can't (i.e. not enough bytes remain), it hands off
to the next type class which handles the next smaller machine word.

I did a quick Core check and found that GHC seems to successfully generate
minimal code for this e.g. for an 8-byte magic, GHC will do one
@writeWord64OffAddr#@ of a constant. Great!
-}

{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module Bytezap.Struct.TypeLits where

import Raehik.TypeLevelBytes
import Bytezap.Struct ( Poke, sequencePokes, emptyPoke, prim )
import Numeric.Natural ( Natural )

-- | Serialize a type-level bytestring, largest grouping 'Word64'.
class ReifyBytesW64 (ns :: [Natural]) where reifyBytesW64 :: Poke s

-- | Enough bytes to make a 'Word64'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 n1
  , ReifyW8 n2
  , ReifyW8 n3
  , ReifyW8 n4
  , ReifyW8 n5
  , ReifyW8 n6
  , ReifyW8 n7
  , ReifyW8 n8
  , ReifyBytesW64 ns
  ) => ReifyBytesW64 (n1 ': n2 ': n3 ': n4 ': n5 ': n6 ': n7 ': n8 ': ns) where
    {-# INLINE reifyBytesW64 #-}
    reifyBytesW64 = sequencePokes
        (prim (reifyW64 @n1 @n2 @n3 @n4 @n5 @n6 @n7 @n8)) 8 (reifyBytesW64 @ns)

-- | Try to group 'Word32's next.
instance ReifyBytesW32 ns => ReifyBytesW64 ns where
    {-# INLINE reifyBytesW64 #-}
    reifyBytesW64 = reifyBytesW32 @ns

-- | Serialize a type-level bytestring, largest grouping 'Word32'.
class ReifyBytesW32 (ns :: [Natural]) where reifyBytesW32 :: Poke s

-- | Enough bytes to make a 'Word32'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 n1
  , ReifyW8 n2
  , ReifyW8 n3
  , ReifyW8 n4
  , ReifyBytesW32 ns
  ) => ReifyBytesW32 (n1 ': n2 ': n3 ': n4 ': ns) where
    {-# INLINE reifyBytesW32 #-}
    reifyBytesW32 = sequencePokes
        (prim (reifyW32 @n1 @n2 @n3 @n4)) 4 (reifyBytesW32 @ns)

-- | Try to group 'Word16's next.
instance ReifyBytesW16 ns => ReifyBytesW32 ns where
    {-# INLINE reifyBytesW32 #-}
    reifyBytesW32 = reifyBytesW16 @ns

-- | Serialize a type-level bytestring, largest grouping 'Word32'.
class ReifyBytesW16 (ns :: [Natural]) where reifyBytesW16 :: Poke s

-- | Enough bytes to make a 'Word16'.
instance
  ( ReifyW8 n1
  , ReifyW8 n2
  , ReifyBytesW16 ns
  ) => ReifyBytesW16 (n1 ': n2 ': ns) where
    {-# INLINE reifyBytesW16 #-}
    reifyBytesW16 = sequencePokes
        (prim (reifyW16 @n1 @n2)) 2 (reifyBytesW16 @ns)

-- | Reify byte-by-byte next.
instance ReifyBytesW8 ns => ReifyBytesW16 ns where
    {-# INLINE reifyBytesW16 #-}
    reifyBytesW16 = reifyBytesW8 @ns

-- | Serialize a type-level bytestring, byte-by-byte.
class ReifyBytesW8 (ns :: [Natural]) where reifyBytesW8 :: Poke s

-- | Reify the next byte.
instance
  ( ReifyW8 n1
  , ReifyBytesW8 ns
  ) => ReifyBytesW8 (n1 ': ns) where
    {-# INLINE reifyBytesW8 #-}
    reifyBytesW8 = sequencePokes
        (prim (reifyW8 @n1)) 1 (reifyBytesW8 @ns)

-- | End of the line.
instance ReifyBytesW8 '[] where
    {-# INLINE reifyBytesW8 #-}
    reifyBytesW8 = emptyPoke
