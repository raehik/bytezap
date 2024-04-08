{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)
-- {-# LANGUAGE FlexibleInstances #-}

{- | Struct parser.

We do still have to do failure checking, because unlike C we check some types
(e.g. bitfields). Hopefully inlining can remove those checks when unnecessary.
-}

module Bytezap.Parser.Struct where

import GHC.Exts
import GHC.ForeignPtr
import Data.Void ( Void )

import Data.Word ( Word8 )
import Data.ByteString.Internal qualified as B
import System.IO.Unsafe ( unsafePerformIO )

import Raehik.Compat.Data.Primitive.Types

type PureMode = Proxy# Void
type IOMode   = State# RealWorld
type STMode s = State# s

type ParserT# (st :: ZeroBitType) e a =
       ForeignPtrContents {- ^ pointer provenance -}
    -> Addr# {- ^ base address -}
    -> Int#  {- ^ cursor offset from base -}
    -> st    {- ^ state token -}
    -> Res# st e a

-- we take a 'ForeignPtrContents' because it lets us create bytestrings without
-- copying if we want. it's useful
newtype ParserT (st :: ZeroBitType) e a =
    ParserT { runParserT# :: ParserT# st e a }

instance Functor (ParserT st e) where
  fmap f (ParserT g) = ParserT \fpc base os st0 -> case g fpc base os st0 of
    OK# st1 a -> let !b = f a in OK# st1 b
    x         -> unsafeCoerce# x
  {-# inline fmap #-}

-- No Applicative due to no offset passing.

-- | The type of pure parsers.
type Parser     = ParserT PureMode

-- | The type of parsers which can embed `IO` actions.
type ParserIO   = ParserT IOMode

-- | The type of parsers which can embed `ST` actions.
type ParserST s = ParserT (STMode s)

-- | Primitive parser result wrapped with a state token.
--
-- You should rarely need to manipulate values of this type directly. Use the
-- provided bidirectional pattern synonyms 'OK#', 'Fail#' and 'Err#'.
type Res# (st :: ZeroBitType) e a =
  (# st, ResI# e a #)

-- | Primitive parser result.
type ResI# e a =
  (#
    (# a #)
  | (# #)
  | (# e #)
  #)

-- | 'Res#' constructor for a successful parse.
--   Contains the return value and a state token.
pattern OK# :: (st :: ZeroBitType) -> a -> Res# st e a
pattern OK# st a = (# st, (# (# a #) | | #) #)

-- | 'Res#' constructor for recoverable failure.
--   Contains only a state token.
pattern Fail# :: (st :: ZeroBitType) -> Res# st e a
pattern Fail# st = (# st, (# | (# #) | #) #)

-- | 'Res#' constructor for errors which are by default non-recoverable.
--    Contains the error, plus a state token.
pattern Err# :: (st :: ZeroBitType) -> e -> Res# st e a
pattern Err# st e = (# st, (# | | (# e #) #) #)
{-# complete OK#, Fail#, Err# #-}

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParserBs :: B.ByteString -> Parser e a -> Result e a
unsafeRunParserBs (B.BS fptr _) = unsafeRunParserFPtr fptr

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParserPtr :: Ptr Word8 -> Parser e a -> Result e a
unsafeRunParserPtr (Ptr base#) = unsafeRunParser' base# FinalPtr

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParserFPtr :: ForeignPtr Word8 -> Parser e a -> Result e a
unsafeRunParserFPtr fptr p =
    unsafePerformIO $ B.unsafeWithForeignPtr fptr $ \ptr ->
        pure $ unsafeRunParserPtr ptr p

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParser' :: Addr# -> ForeignPtrContents -> Parser e a -> Result e a
unsafeRunParser' base# fpc (ParserT p) =
    case p fpc base# 0# proxy# of
      OK#   _st1 a -> OK a
      Fail# _st1   -> Fail
      Err#  _st1 e -> Err e

-- | Higher-level boxed data type for parsing results.
data Result e a =
    OK a    -- ^ Contains return value.
  | Fail    -- ^ Recoverable-by-default failure.
  | Err !e  -- ^ Unrecoverable-by-default error.
  deriving Show

-- | can't provide via 'pure' as no 'Applicative'
constParse :: a -> ParserT st e a
constParse a = ParserT \_fpc _base _os st -> OK# st a

sequenceParsers
    :: Int -> (a -> b -> c)
    -> ParserT st e a -> ParserT st e b -> ParserT st e c
sequenceParsers (I# len#) f (ParserT pa) (ParserT pb) =
    ParserT \fpc base os# st0 ->
        case pa fpc base os# st0 of
          Fail# st1 ->  Fail# st1
          Err# st1 e -> Err# st1 e
          OK# st1 a ->
            case pb fpc base (os# +# len#) st1 of
              Fail# st2 ->  Fail# st2
              Err# st2 e -> Err# st2 e
              OK# st2 b -> OK# st2 (f a b)

-- TODO using indexWord8OffAddrAs to permit pure mode. flatparse does this (at
-- least for integers). guess it's OK?
prim :: forall a st e. Prim' a => ParserT st e a
prim = ParserT \_fpc base os st ->
    case indexWord8OffAddrAs# base os of a -> OK# st a
