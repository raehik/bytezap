{- | Efficient type-level bytestring parsing.

One may implement this using the type-level serializing, but mirroring it for
parsing does less work and allocation.
-}

{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module Bytezap.Parser.Struct.TypeLits.Bytes where

import Raehik.TypeLevelBytes
import Bytezap.Parser.Struct ( ParserT, prim, withLit, constParse )
import Numeric.Natural ( Natural )

class ParseReifyBytesW64 (ns :: [Natural]) where
    parseReifyBytesW64 :: ParserT st e ()

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
  , ParseReifyBytesW64 ns
  ) => ParseReifyBytesW64 (n1 ': n2 ': n3 ': n4 ': n5 ': n6 ': n7 ': n8 ': ns) where
    {-# INLINE parseReifyBytesW64 #-}
    parseReifyBytesW64 = withLit 8# (reifyW64 @n1 @n2 @n3 @n4 @n5 @n6 @n7 @n8)
        prim (parseReifyBytesW64 @ns)

-- | Try to group 'Word32's next.
instance ParseReifyBytesW32 ns => ParseReifyBytesW64 ns where
    {-# INLINE parseReifyBytesW64 #-}
    parseReifyBytesW64 = parseReifyBytesW32 @ns

-- | Serialize a type-level bytestring, largest grouping 'Word32'.
class ParseReifyBytesW32 (ns :: [Natural]) where
    parseReifyBytesW32 :: ParserT st e ()

-- | Enough bytes to make a 'Word32'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 n1
  , ReifyW8 n2
  , ReifyW8 n3
  , ReifyW8 n4
  , ParseReifyBytesW32 ns
  ) => ParseReifyBytesW32 (n1 ': n2 ': n3 ': n4 ': ns) where
    {-# INLINE parseReifyBytesW32 #-}
    parseReifyBytesW32 = withLit 4# (reifyW32 @n1 @n2 @n3 @n4)
        prim (parseReifyBytesW32 @ns)

-- | Try to group 'Word16's next.
instance ParseReifyBytesW16 ns => ParseReifyBytesW32 ns where
    {-# INLINE parseReifyBytesW32 #-}
    parseReifyBytesW32 = parseReifyBytesW16 @ns

-- | Serialize a type-level bytestring, largest grouping 'Word16'.
class ParseReifyBytesW16 (ns :: [Natural]) where
    parseReifyBytesW16 :: ParserT st e ()

-- | Enough bytes to make a 'Word16'.
instance
  ( ReifyW8 n1
  , ReifyW8 n2
  , ParseReifyBytesW16 ns
  ) => ParseReifyBytesW16 (n1 ': n2 ': ns) where
    {-# INLINE parseReifyBytesW16 #-}
    parseReifyBytesW16 = withLit 2# (reifyW16 @n1 @n2)
        prim (parseReifyBytesW16 @ns)

-- | Reify byte-by-byte next.
instance ParseReifyBytesW8 ns => ParseReifyBytesW16 ns where
    {-# INLINE parseReifyBytesW16 #-}
    parseReifyBytesW16 = parseReifyBytesW8 @ns

-- | Serialize a type-level bytestring, byte-by-byte.
class ParseReifyBytesW8 (ns :: [Natural]) where
    parseReifyBytesW8 :: ParserT st e ()

-- | Reify the next byte.
instance
  ( ReifyW8 n1
  , ParseReifyBytesW8 ns
  ) => ParseReifyBytesW8 (n1 ': ns) where
    {-# INLINE parseReifyBytesW8 #-}
    parseReifyBytesW8 = withLit 1# (reifyW8 @n1)
        prim (parseReifyBytesW8 @ns)

-- | End of the line.
instance ParseReifyBytesW8 '[] where
    {-# INLINE parseReifyBytesW8 #-}
    parseReifyBytesW8 = constParse ()
