-- {-# LANGUAGE UndecidableInstances #-}

module Store2.Primitive where

import Data.Int
import Data.Word
import Numeric.Natural

import GHC.Generics ( Generic )

-- TODO no, define in a single newtype rather than data family. doesn't get us
-- anything.
data End = LE | BE

data ISign = U | I

data family N (isign :: ISign) (isize :: Natural) (end :: End)
newtype instance N 'U 8  end = U8  { unU8  :: Word8  }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Word8
newtype instance N 'I 8  end = I8  { unI8  :: Int8   }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Int8
newtype instance N 'U 16 end = U16 { unU16 :: Word16 }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Word16
newtype instance N 'I 16 end = I16 { unI16 :: Int16  }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Int16
newtype instance N 'U 32 end = U32 { unU32 :: Word32 }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Word32
newtype instance N 'I 32 end = I32 { unI32 :: Int32  }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Int32
newtype instance N 'U 64 end = U64 { unU64 :: Word64 }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Word64
newtype instance N 'I 64 end = I64 { unI64 :: Int64  }
    deriving stock (Show, Generic)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via Int64
