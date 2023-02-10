{-# LANGUAGE CPP #-}

module Store2 where

import Foreign

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

newtype Serialize (a :: k) =
    Serialize { Serialize :: Addr# -> a -> State# d -> State# d }

-- adapted from @Data.ByteString.Builder.Prim.Internal.storableToF@
{-# INLINE CONLIKE putStorable #-}
putStorable :: forall a. Storable a => a -> Ptr Word8 -> IO ()
-- Not all architectures are forgiving of unaligned accesses; whitelist ones
-- which are known not to trap (either to the kernel for emulation, or crash).
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
