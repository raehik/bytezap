module TmpSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Generic.Random
import Test.QuickCheck

import Data.Text qualified as T
import Test.QuickCheck.Instances.Text()
import Bytezap.Json

spec :: Spec
spec = modifyMaxSuccess (const 10000000) $ do
    prop "escapedLength8 === escapedLength64" $ do
      \(t :: T.Text) -> escapedLength8 t `shouldBe` escapedLength64 t
