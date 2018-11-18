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

import           Data.Functor.Identity         (Identity)
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

data CartUsersT f = CartUsers {
  _Email         :: Columnar f Text,
  _First_name    :: Columnar f Text,
  _Last_name     :: Columnar f Text,
  _Is_member     :: Columnar f Bool,
  _Days_in_queue :: Columnar f Int
} deriving (G.Generic)

type CartUsers = CartUsersT Identity
deriving instance Show CartUsers
instance S.Generic CartUsers
instance S.HasDatatypeInfo CartUsers


type family Columnar (f :: * -> *) x where
    Columnar Identity x = x
    Columnar f x = f x

data SubsetCartUsersT f = SubsetCartUsers {
  _Email      :: Columnar f Text,
  _First_name :: Columnar f Text,
  _Last_name  :: Columnar f Text
} deriving (G.Generic)

type SubsetUsers = SubsetCartUsersT Identity
instance S.Generic SubsetUsers
instance S.HasDatatypeInfo SubsetUsers
deriving instance Show SubsetUsers
deriving instance Eq SubsetUsers

data SupersetCartUsersT f = SupersetCartUsers {
  _Email         :: Columnar f Text,
  _First_name    :: Columnar f Text,
  _Last_name     :: Columnar f Text,
  _Is_member     :: Columnar f Bool,
  _Days_in_queue :: Columnar f Int,
  _Zipcode       :: Columnar f Text,
  _City          :: Columnar f Text,
  _Country       :: Columnar f Text
} deriving (G.Generic)

type SupersetUsers = SupersetCartUsersT Identity
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
r1 = MPR { age = 23, iscool = True, yearbook = "You spin me right round"}

subset ::
  Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
  -> Rec ElField '[("age" ::: Int), ("yearbook" ::: Text)]
subset = rcast


r2 :: Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
r2 = xrec (23, True, "You spin me right round")

r3 :: MySubsetRecord
r3 = MSR {age = 23, yearbook = "You spin me right round"}

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
    , _Zipcode       = "ABCD1234"
    , _City          = "ABC"
    , _Country = "XYZ"
  }
