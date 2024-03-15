module Bytezap.Write.Derived where

import Bytezap.Write.Internal
import Bytezap.Poke.Derived qualified as P

import Data.ByteString.Short qualified as SBS
import Data.Text.Internal qualified as T
import Data.Char ( ord )

-- | Write a 'SBS.ShortByteString'.
shortByteString :: SBS.ShortByteString -> Write s
shortByteString sbs = Write (SBS.length sbs) (P.shortByteString sbs)

-- | Write a 'T.Text'.
text :: T.Text -> Write s
text t@(T.Text _arr _off len) = Write len (P.text t)

-- | Write a 'Char'.
--
-- Adapted from utf8-string.
char :: Char -> Write s
char c = Write (go (ord c)) (P.char c)
 where
  go oc
   | oc <= 0x7f       = 1
   | oc <= 0x7ff      = 2
   | oc <= 0xffff     = 3
   | otherwise        = 4
