{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Store2 where

import GHC.Exts
import GHC.IO
import Data.Kind
import Data.Void
import Data.Word
import Data.Int
import Foreign
import GHC.ForeignPtr

import Data.ByteString ( ByteString )
import Data.ByteString.Internal qualified as B

import GHC.Generics
import GHC.Generics qualified as Generics

newtype Serializer# s (a :: TYPE r) =
    Serializer# { runSerializer# :: Serializer'# s a }

type Serializer'# s (a :: TYPE r) = Addr# -> a -> State# s -> State# s

class Serialize (a :: TYPE r) where
    serialize :: Serializer'# RealWorld a
    serializedLen :: a -> Int#
    serializedLen' :: SerializedLen a

instance Serialize Word8# where
    serialize = sW8#
    serializedLen _ = 1#
    serializedLen' = SLenConst 1#

instance Serialize Word# where
    serialize = sW#
    serializedLen _ = 8#
    serializedLen' = SLenConst 8#

instance Serialize Int where
    serialize = sI
    serializedLen _ = 8#
    serializedLen' = SLenConst 8#

instance Serialize Word where
    serialize = sW
    serializedLen _ = 8#
    serializedLen' = SLenConst 8#

instance (Serialize a, Serialize b) => Serialize (a, b) where
    serialize = undefined
    serializedLen (a, b) = serializedLen a +# serializedLen b
    serializedLen' = addSerializedLen serializedLen' serializedLen'

-- v can't do this, no raw unboxed values
--type SerializedLen (a :: TYPE r) = (# Int# | a -> Int# #)

data SerializedLen (a :: TYPE r) = SLenConst Int# | SLenCalc (a -> Int#)
instance Show (SerializedLen a) where
    show = \case
      SLenConst i -> "SLenConst "<>show (I# i)

addSerializedLen
    :: SerializedLen a -> SerializedLen b -> SerializedLen (a, b)
addSerializedLen sll slr =
    case sll of
      SLenConst sllC ->
        case slr of
          SLenConst slrC -> SLenConst (sllC +# slrC)
          SLenCalc  slrF -> SLenCalc $ \(_l, r) -> sllC   +# slrF r
      SLenCalc  sllF ->
        case slr of
          SLenConst slrC -> SLenCalc $ \(l, _r) -> slrC   +# sllF l
          SLenCalc  slrF -> SLenCalc $ \(l,  r) -> sllF l +# slrF r

{-# inline sW8# #-}
sW8# :: Serializer'# s Word8#
sW8# addr# w8# st = writeWord8OffAddr# addr# 0# w8# st

{-# inline sW# #-}
sW# :: Serializer'# s Word#
sW# addr# w# st = writeWordOffAddr# addr# 0# w# st

{-# inline sI #-}
sI :: Serializer'# s Int
sI addr# (I# i#) st = writeIntOffAddr# addr# 0# i# st

{-# inline sW #-}
sW :: Serializer'# s Word
sW addr# (W# w#) st = writeWordOffAddr# addr# 0# w# st

{-# inline sBS #-}
sBS :: Serializer'# RealWorld ByteString
sBS addr# (B.BS bsFp len) st =
    unIO_ (unsafeWithForeignPtr bsFp $ \bsP -> copyBytes (Ptr addr#) bsP len) st

{-# inline unIO_ #-}
unIO_ :: IO a -> State# RealWorld -> State# RealWorld
unIO_ (IO f) rw = case f rw of (# rw', _a #) -> rw'

s' :: Int -> Serializer# RealWorld a -> a -> ByteString
s' len s# a = B.unsafeCreate len (liftSerializer# s# a)

s :: (a -> Int) -> Serializer# RealWorld a -> a -> ByteString
s fLen s# a = B.unsafeCreate (fLen a) (liftSerializer# s# a)

{-# inline liftSerializer# #-}
-- | Lift a 'Serializer#' into its corresponding @'IO' '()'@ action.
liftSerializer# :: Serializer# RealWorld a -> a -> Ptr Word8 -> IO ()
liftSerializer# (Serializer# f#) a (Ptr p#) = IO $ \rw# ->
    (# f# p# a rw#, () #)

newtype Ptr# a = Ptr# Addr#

-- from bytestring (e.g. @Data.ByteString.Builder.Prim.Internal.storableToF@)
-- Not all architectures are forgiving of unaligned accesses; whitelist ones
-- which are known not to trap (either to the kernel for emulation, or crash).
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH) \
    || ((defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)) \
        && defined(__ARM_FEATURE_UNALIGNED)) \
    || defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(powerpc64le_HOST_ARCH)
#define SAFE_UNALIGNED
#endif

{-

-- adapted from @Data.ByteString.Builder.Prim.Internal.storableToF@
{-# INLINE CONLIKE putStorable #-}
putStorable :: forall a. Storable a => a -> Ptr Word8 -> IO ()
#ifdef SAFE_UNALIGNED
putStorable a p = poke (castPtr p) a
#else
putStorable a p =
    if ptrToWordPtr p `mod` fromIntegral (alignment' @a) == 0 then poke (castPtr p) a
    else with a $ \tp -> copyBytes p (castPtr tp) (sizeOf' @a)
#endif

{-# INLINE CONLIKE storableToF #-}
storableToF :: forall a. Storable a => FixedPrim a
#ifdef SAFE_UNALIGNED
storableToF = FP (sizeOf (undefined :: a)) (\x op -> poke (castPtr op) x)
#else
storableToF = FP (sizeOf (undefined :: a)) $ \x op ->
    if ptrToWordPtr op `mod` fromIntegral (alignment (undefined :: a)) == 0 then poke (castPtr op) x
    else with x $ \tp -> copyBytes op (castPtr tp) (sizeOf (undefined :: a))
#endif

-}

#if !MIN_VERSION_base(4,17,0)
{-
GHC 9.4 clarified the story for types without runtime representations. These
type synonyms are defined and used to simplify certain internal definitions
(e.g. 'State#'). They are nicer than using the "expanded" type, so we define
them here for older compilers.
-}
type ZeroBitRep = 'TupleRep ('[] :: [RuntimeRep])
type ZeroBitType = TYPE ZeroBitRep
#endif

{-# inline serializeGeneric #-}
serializeGeneric :: (Generic a, GSerialize (Rep a)) => Serializer'# RealWorld a
serializeGeneric addr# a st = gserialize addr# (Generics.from a) st

class GSerialize f where
    gserialize     :: Serializer'# RealWorld (f p)
    gserializedLen :: f p -> Int#

instance GSerialize U1 where
    gserialize _addr# U1 st = st
    gserializedLen U1 = 0#

instance Serialize c => GSerialize (K1 i c) where
    gserialize addr# (K1 c) st = serialize addr# c st
    gserializedLen (K1 c) = serializedLen c

instance (GSerialize l, GSerialize r) => GSerialize (l :*: r) where
    gserialize addr# (l :*: r) st =
        let st'  = gserialize addr# l st
            st'' = gserialize (addr# `plusAddr#` (gserializedLen l)) r st'
        in  st''
    gserializedLen (l :*: r) = gserializedLen l +# gserializedLen r
