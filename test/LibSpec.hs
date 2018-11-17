{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LibSpec where

import           Data.Text
import           Data.Vinyl
import           Data.Vinyl.Generics.Transform
import qualified Generics.SOP                  as S
import qualified GHC.Generics                  as G
import           Test.Hspec

data MyPlainRecord = MPR {
  age      :: Int,
  iscool   :: Bool,
  yearbook :: Text
} deriving (Show, G.Generic)

instance S.Generic MyPlainRecord
instance S.HasDatatypeInfo MyPlainRecord

data MySubsetRecord = MSR {
  age      :: Int,
  yearbook :: Text
} deriving (Eq, Show, G.Generic)

instance S.Generic MySubsetRecord

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    it "test1" $ do
      (toVinyl r1) `shouldBe` r2
    it "test2" $ do
      (fromVinyl $ subset (toVinyl r1)) `shouldBe` r3

r1 :: MyPlainRecord
r1 = MPR { age = 23, iscool = True, yearbook = "You spin me right round"}

subset ::
  Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
  -> Rec ElField '[("age" ::: Int), ("yearbook" ::: Text)]
subset = rcast


r2 :: Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
r2 = xrec (23, True, "You spin me right round")

r3 :: MySubsetRecord
r3 = MSR {age = 23, yearbook = "You spin me right round"}
