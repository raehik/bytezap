{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- thanks to type manipulation

module Bytezap.Parser.Struct.Generic where

import Bytezap.Parser.Struct
import GHC.Generics
import GHC.Exts
import Data.Kind
import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Bytezap.Common.Generic ( type GTFoldMapCAddition )
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

    -- | Defunctionalization symbol for a type family turning 'Type's into
    --   'Natural's. (Needed as we can't partially apply type families.)
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
  , lenL ~ GTFoldMapCAddition (GParseBaseLenTF tag) l
  , KnownNat lenL
  ) => GParse tag (l :*: r) where
    gParse = sequenceParsers len (:*:) (gParse @tag) (gParse @tag)
      where
        len = natValInt @lenL

instance (GParseBase tag, GParseBaseC tag a) => GParse tag (S1 c (Rec0 a)) where
    gParse = (M1 . K1) <$> gParseBase @tag

-- | Wow, look! Nothing!
instance GParse tag U1 where gParse = constParse U1
