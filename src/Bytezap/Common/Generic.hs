{-# LANGUAGE UndecidableInstances #-} -- due to type family nesting

module Bytezap.Common.Generic where

import GHC.Generics
import GHC.TypeNats
import Data.Kind
import DeFun.Core ( type (~>), type (@@) )

type family GCstrLen (toLen :: Type ~> Natural) (gf :: k -> Type) :: Natural where
    GCstrLen _     U1          = 0
    GCstrLen toLen (K1 i c)    = toLen @@ c
    GCstrLen toLen (l :*: r)   = GCstrLen toLen l + GCstrLen toLen r
    GCstrLen toLen (M1 _ _ gf) = GCstrLen toLen gf
