module Bytezap.Struct.Generic.Enum where

import Bytezap.Struct
import GHC.Generics
import GHC.Exts
import Bytezap.Common.Generic ( type GTFoldMapCAddition )
import Data.Kind
import GHC.TypeNats
import Util.TypeNats ( natValInt )
import DeFun.Core ( type (~>) )

class GPokeBaseEnum' sumtag where
    type GPokeBaseEnumCstrTy sumtag
class GPokeBaseEnum sumtag where
    type GPokeBaseEnumCstrTo sumtag (sym :: Symbol) :: GPokeBaseEnumCstrTy sumtag
    type GPokeBaseEnumC sumtag :: Constraint

class GPokeEnum tag sumtag gf where
    gPokeEnum :: gf p -> Poke# (GPokeBaseSt tag)

instance GPokeEnum tag sumtag gf => GPoke tag (D1 c gf) where
    gPokeEnum = gPokeEnum @tag @sumtag . unM1

-- urgh gonna be gross as always.
-- probably best to hand off to another class when we see (:+:)

instance GPoke tag (C1 c U1) where
    gPoke = _

{-

instance
  ( GPoke tag l
  , GPoke tag r
  , GPokeBase tag
  , lenL ~ GTFoldMapCAddition (GPokeBaseLenTF tag) l
  , KnownNat lenL
  ) => GPoke tag (l :*: r) where
    -- TODO moved os and s0 to RHS because base is const and those aren't?
    -- will this change anything?? idk!!!!
    gPoke (l :*: r) base# = \os# s0 ->
        case gPoke @tag l base# os# s0 of
          s1 -> gPoke @tag r base# (os# +# lenL#) s1
      where
        !(I# lenL#) = natValInt @lenL

instance (GPokeBase tag, GPokeBaseC tag a) => GPoke tag (S1 c (Rec0 a)) where
    gPoke = gPokeBase @tag . unK1 . unM1

-- | Wow, look! Nothing!
instance GPoke tag U1 where gPoke U1 _base# = \_os# s0 -> s0

-}
