{-# LANGUAGE UnboxedTuples #-}

module Bytezap.Text where

import Bytezap
import Bytezap.Bytes

import Data.Text.Internal
import Data.Text.Array qualified as A
import GHC.Exts

textUtf8 :: Text -> Write
textUtf8 (Text (A.ByteArray arr#) (I# off#) len@(I# len#)) =
    Write len $ pokeByteArray# arr# off# len#
{-# INLINE textUtf8 #-}
