{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LibSpec where

import           Test.Hspec
import           Test.Hspec.Core.Util

import           Data.Text
import           Data.Vinyl
import qualified Data.Vinyl.Functor            as VF
import           Data.Vinyl.Generics.Transform
import           Generics.SOP
import qualified Generics.SOP                  as S
import           GHC.Exception                 (SomeException)
import qualified GHC.Generics                  as G

data MyPlainRecord f = MPR {
  age      :: Int,
  iscool   :: Bool,
  yearbook :: Text
} deriving (Show, G.Generic)

instance S.Generic (MyPlainRecord f)
deriveVinyl ''MyPlainRecord

data MySubsetRecord f = MSR {
  _age      :: Int,
  _yearbook :: Text
} deriving (Show, G.Generic)

instance S.Generic (MySubsetRecord f)
deriveVinyl ''MySubsetRecord

derivePlain "_"

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    it "test1" $ do
      res <- safeTry $ print test1
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "test2" $ do
      res <- safeTry $ print test2
      (formatEx res) `shouldBe` (NoExceptionRaised)


data ExceptionStatus = ExceptionRaised String | NoExceptionRaised deriving (Show, Eq)

formatEx :: Either SomeException b -> ExceptionStatus
formatEx (Left e)  = ExceptionRaised (formatException e)
formatEx (Right _) = NoExceptionRaised

r1 :: MyPlainRecord Int
r1 = MPR { age = 23, iscool = True, yearbook = "You spin me right round"}

test1 = toRecElField r1

subset ::
  Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
  -> Rec ElField '[("age" ::: Int), ("yearbook" ::: Text)]
subset = rcast

test2 :: MySubsetRecord Int
test2 = toPlainRecord $ subset test1
