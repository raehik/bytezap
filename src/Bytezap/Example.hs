module Bytezap.Example where

import Bytezap qualified as Bytezap
import Bytezap.Class qualified as Bytezap
import Bytezap.Class.Generic qualified as Bytezap

import GHC.Generics ( Generic )
import Data.Word
import Data.ByteString ( ByteString )

data D1 = D1 Word8 | D2 Word16 | D3 Word8 Word8 Word32 | D4 | D5 ByteString
    deriving stock (Generic, Show, Eq)

instance Bytezap.Put D1 where
    put = Bytezap.putGeneric
