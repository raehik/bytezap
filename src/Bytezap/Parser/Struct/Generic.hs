{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- thanks to type manipulation

-- unlike the serializer we stay newtyped because we want our Functor

module Bytezap.Parser.Struct.Generic where

import Bytezap.Parser.Struct
import GHC.Generics
import GHC.Exts
import Data.Kind

-- TODO fill in kind of a (some generic thing)
type family UnwrapGenericS1 a where
    UnwrapGenericS1 (S1 c (Rec0 a)) = a

class GParseBase tag where
    -- | The state token of the parser.
    type GParseBaseSt tag :: ZeroBitType
    type GParseBaseC tag a :: Constraint
    type GParseBaseE tag :: Type
    gParseBase
        :: GParseBaseC tag a
        => ParserT (GParseBaseSt tag) (GParseBaseE tag) a

    -- | The type class that provides parse length (known at compile time).
    type GParseBaseLen tag a :: Constraint

    -- | Get the poked length of the given type.
    --
    -- I think we have to pass a proxy, because of forall limitations on
    -- instance signatures. This would be much better with explicit type
    -- variables (GHC 9.10 or 9.12).
    gParseBaseLen :: forall a. GParseBaseLen tag a => Proxy# a -> Int

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
  , GParseBaseLen tag (UnwrapGenericS1 l)
  ) => GParse tag (l :*: r) where
    gParse = sequenceParsers len (:*:) (gParse @tag) (gParse @tag)
      where
        len = gParseBaseLen @tag (proxy# @(UnwrapGenericS1 l))

instance (GParseBase tag, GParseBaseC tag a) => GParse tag (S1 c (Rec0 a)) where
    gParse = (M1 . K1) <$> gParseBase @tag

-- | Wow, look! Nothing!
instance GParse tag U1 where gParse = constParse U1
