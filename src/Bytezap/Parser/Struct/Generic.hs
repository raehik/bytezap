{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- thanks to type manipulation

module Bytezap.Parser.Struct.Generic where

import Bytezap.Parser.Struct
import GHC.Generics
import GHC.Exts
import Data.Kind
import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Bytezap.Common.Generic ( type GCstrLen )
import DeFun.Core ( type (~>) )

class GParseBase tag where
    -- | The state token of the parser.
    type GParseBaseSt tag :: ZeroBitType
    type GParseBaseC tag a :: Constraint
    type GParseBaseE tag :: Type

    -- unlike the serializer we stay newtyped because we want our Functor
    gParseBase
        :: GParseBaseC tag a
        => ParserT (GParseBaseSt tag) (GParseBaseE tag) a

    type GParseBaseLenTF tag :: Type ~> Natural

class GParse tag gf where
    gParse :: ParserT (GParseBaseSt tag) (GParseBaseE tag) (gf p)

instance GParse tag gf => GParse tag (D1 cd gf) where
    gParse = M1 <$> gParse @tag
instance GParse tag gf => GParse tag (C1 cc gf) where
    gParse = M1 <$> gParse @tag

instance
  ( GParse tag l
  , GParse tag r
  , GParseBase tag
  , KnownNat (GCstrLen (GParseBaseLenTF tag) l)
  ) => GParse tag (l :*: r) where
    gParse = sequenceParsers len (:*:) (gParse @tag) (gParse @tag)
      where
        len = natValInt @(GCstrLen (GParseBaseLenTF tag) l)

instance (GParseBase tag, GParseBaseC tag a) => GParse tag (S1 c (Rec0 a)) where
    gParse = (M1 . K1) <$> gParseBase @tag

-- | Wow, look! Nothing!
instance GParse tag U1 where gParse = constParse U1
