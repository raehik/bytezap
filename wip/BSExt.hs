{-# LANGUAGE UnboxedTuples #-}

-- | raehik's bytestring extras (reimplementations of unexported internals).

module Tmp.BSExt
  ( module Tmp.BSExt
  , B.mkDeferredByteString
  ) where

import GHC.ForeignPtr ( ForeignPtr, unsafeWithForeignPtr, withForeignPtr )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal.Utils ( copyBytes )
import Data.ByteString.Internal qualified as B
import Data.ByteString ( ByteString )
import Data.Word ( Word8 )
import Control.Exception ( assert )
import GHC.IO ( IO(IO) )
import GHC.Exts ( runRW# )

-- | Copy the given number of bytes from the second area (source) into the first
--   (destination); the copied areas may not overlap.
--
-- Reimplemented from the unexported function
-- 'Data.ByteString.Internal.Type.memcpyFp'.
memcpyFp :: ForeignPtr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyFp fp fq s = unsafeWithForeignPtr fp $ \p ->
                     unsafeWithForeignPtr fq $ \q -> copyBytes p q s

-- | Create a 'ByteString' of size @l@ and use action @f@ to fill its contents.
--
-- Reimplemented from the unexported function
-- 'Data.ByteString.Internal.Type.createFp.
createFp :: Int -> (ForeignPtr Word8 -> IO ()) -> IO ByteString
createFp len action = assert (len >= 0) $ do
    fp <- B.mallocByteString len
    action fp
    B.mkDeferredByteString fp len
{-# INLINE createFp #-}

createUptoNCPS
    :: Int
    -> (Ptr Word8 -> (Int -> IO ByteString) -> IO r)
    -> IO r
createUptoNCPS maxLen action = assert (maxLen >= 0) $ do
    fp <- B.mallocByteString maxLen
    withForeignPtr fp $ \p -> action p $ \len ->
        B.mkDeferredByteString fp len
{-# INLINE createUptoNCPS #-}

createCPS
    :: Int
    -> (ForeignPtr Word8 -> Int -> IO r)
    -> ((Int -> IO r) -> Ptr Word8 -> IO r)
    -> IO r
createCPS maxLen finalize f = assert (maxLen >= 0) $ do
    fp <- B.mallocByteString maxLen
    withForeignPtr fp $ \buf -> f (finalize fp) buf
{-# INLINE createCPS #-}

{-
withBuffer
    :: Int
    -> (Ptr Word8 -> Int -> IO r)
    -> (r ->
withBuffer bufLen
{-# INLINE withBuffer #-}
-}

createAndTrimCPS
    :: Int
    -> (Ptr Word8 -> (Int -> IO ByteString) -> IO r)
    -> IO r
createAndTrimCPS maxLen action = assert (maxLen >= 0) $ do
    fp <- B.mallocByteString maxLen
    withForeignPtr fp $ \p -> action p $ \len ->
        if   len < maxLen
        then createFp len (\fp' -> memcpyFp fp' fp len)
             -- ^ apparently @fp@ will get GCed automatically, up to GHC
        else B.mkDeferredByteString fp maxLen
{-# INLINE createAndTrimCPS #-}

unsafeCreateAndTrimCPS
    :: Int
    -> (Ptr Word8 -> (Int -> IO ByteString) -> IO r)
    -> r
unsafeCreateAndTrimCPS l f =
    unsafeDupablePerformIOByteString (createAndTrimCPS l f)
{-# INLINE unsafeCreateAndTrimCPS #-}

createAndTrimFailable
    :: Int
    -> (Ptr Word8 -> IO (Either e Int))
    -> IO (Either e ByteString)
createAndTrimFailable l action = createFpAndTrimFailable l (wrapAction action)
{-# INLINE createAndTrimFailable #-}

-- TODO how do I omit the Either allocation?
createFpAndTrimFailable
    :: Int
    -> (ForeignPtr Word8 -> IO (Either e Int))
    -> IO (Either e ByteString)
createFpAndTrimFailable maxLen action = assert (maxLen >= 0) $ do
    fp <- B.mallocByteString maxLen
    action fp >>= \case
      Right len ->
        if   len < maxLen
        then Right <$> createFp len (\fp' -> memcpyFp fp' fp len)
             -- ^ apparently @fp@ will get GCed automatically, up to GHC
        else Right <$> B.mkDeferredByteString fp maxLen
      Left  err -> pure $ Left err
{-# INLINE createFpAndTrimFailable #-}

createUptoNFailable
    :: Int
    -> (Ptr Word8 -> IO (Either e Int))
    -> IO (Either e ByteString)
createUptoNFailable l action = createFpUptoNFailable l (wrapAction action)
{-# INLINE createUptoNFailable #-}

createFpUptoNFailable
    :: Int
    -> (ForeignPtr Word8 -> IO (Either e Int))
    -> IO (Either e ByteString)
createFpUptoNFailable maxLen action = assert (maxLen >= 0) $ do
    fp <- B.mallocByteString maxLen
    action fp >>= \case
      Right len -> Right <$> B.mkDeferredByteString fp len
      Left  err -> pure $ Left err
{-# INLINE createFpUptoNFailable #-}

createFailable
    :: Int
    -> (Ptr Word8 -> IO (Either e Int))
    -> IO (Either e ByteString)
createFailable l action = createFpFailable l (wrapAction action)
{-# INLINE createFailable #-}

-- TODO how do I omit the Either allocation?
createFpFailable
    :: Int
    -> (ForeignPtr Word8 -> IO (Either e Int))
    -> IO (Either e ByteString)
createFpFailable maxLen action = assert (maxLen >= 0) $ do
    fp <- B.mallocByteString maxLen
    action fp >>= \case
      Right len ->
        -- TODO does not check for correctness (len <= maxLen)!! don't lie!!!!
        Right <$> B.mkDeferredByteString fp len
      Left  err -> pure $ Left err
{-# INLINE createFpFailable #-}

-- TODO probably don't export
wrapAction :: (Ptr Word8 -> IO res) -> ForeignPtr Word8 -> IO res
wrapAction = flip withForeignPtr
  -- Cannot use unsafeWithForeignPtr, because action can diverge

unsafeDupablePerformIOByteString :: IO a -> a
-- Why does this exist? In base-4.15.1.0 until at least base-4.18.0.0,
-- the version of unsafeDupablePerformIO in base prevents unboxing of
-- its results with an opaque call to GHC.Exts.lazy, for reasons described
-- in Note [unsafePerformIO and strictness] in GHC.IO.Unsafe. (See
-- https://hackage.haskell.org/package/base-4.18.0.0/docs/src/GHC.IO.Unsafe.html#line-30 .)
-- Even if we accept the (very questionable) premise that the sort of
-- function described in that note should work, we expect no such
-- calls to be made in the context of bytestring.  (And we really want
-- unboxing!)
unsafeDupablePerformIOByteString (IO act) =
    case runRW# act of (# _, res #) -> res
