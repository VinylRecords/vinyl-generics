{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module LibSpec where

import           Data.Text
import           Data.Vinyl
import           Data.Vinyl.Generics.Transform
import qualified Generics.SOP                  as S
import qualified Generics.SOP.Record.SubTyping as SRS
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

data CartUsers = CartUsers {
  _Email         ::  Text,
  _First_name    ::  Text,
  _Last_name     ::  Text,
  _Is_member     ::  Bool,
  _Days_in_queue ::  Int
} deriving (G.Generic)

deriving instance Show CartUsers
instance S.Generic CartUsers
instance S.HasDatatypeInfo CartUsers


data SubsetUsers = SubsetCartUsers {
  _Email      ::  Text,
  _First_name ::  Text,
  _Last_name  ::  Text
} deriving (G.Generic)

instance S.Generic SubsetUsers
instance S.HasDatatypeInfo SubsetUsers
deriving instance Show SubsetUsers
deriving instance Eq SubsetUsers

data SupersetUsers = SupersetCartUsers {
  _Email         ::  Text,
  _First_name    ::  Text,
  _Last_name     ::  Text,
  _Is_member     ::  Bool,
  _Days_in_queue ::  Int,
  _Zipcode       ::  Text,
  _City          ::  Text,
  _Country       ::  Text
} deriving (G.Generic)

deriving instance Show SupersetUsers
instance S.Generic SupersetUsers
instance S.HasDatatypeInfo SupersetUsers
deriving instance Eq SupersetUsers

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "test1" $ do
      (toVinyl r1) `shouldBe` r2
    it "test2" $ do
      (fromVinyl $ subset (toVinyl r1)) `shouldBe` r3
    it "test3" $ do
      (SRS.cast r4) `shouldBe` r5
    it "test4" $ do
      r6 `shouldBe` r7


r1 :: MyPlainRecord
r1 = MPR { age = 23, iscool = True, yearbook = "!123!"}

subset ::
  Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
  -> Rec ElField '[("age" ::: Int), ("yearbook" ::: Text)]
subset = rcast


r2 :: Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
r2 = xrec (23, True, "!123!")

r3 :: MySubsetRecord
r3 = MSR {age = 23, yearbook = "!123!"}

r4 :: CartUsers
r4 =
  CartUsers {
       _Email         = "johndoe@foobar.com"
    ,  _First_name    = "John"
    ,  _Last_name     = "Doe"
    ,  _Is_member     = True
    ,  _Days_in_queue = 42
  }

r5 :: SubsetUsers
r5 =
  SubsetCartUsers {
       _Email         = "johndoe@foobar.com"
    ,  _First_name    = "John"
    ,  _Last_name     = "Doe"
  }

additionalFields :: Rec ElField '[("_Zipcode" ::: Text), ("_City" ::: Text), ("_Country" ::: Text)]
additionalFields = xrec ("ABCD1234", "ABC", "XYZ")

r6 :: SupersetUsers
r6 = fromVinyl $ (toVinyl r4) `rappend` additionalFields

r7 :: SupersetUsers
r7 =
  SupersetCartUsers {
       _Email         = "johndoe@foobar.com"
    ,  _First_name    = "John"
    ,  _Last_name     = "Doe"
    ,  _Is_member     = True
    ,  _Days_in_queue = 42
    , _Zipcode        = "ABCD1234"
    , _City           = "ABC"
    , _Country        = "XYZ"
  }


