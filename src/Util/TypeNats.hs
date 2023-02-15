{-# LANGUAGE AllowAmbiguousTypes #-}

module Util.TypeNats where

-- natVal''
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import GHC.Exts ( proxy#, Proxy# )

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal'' #-}
