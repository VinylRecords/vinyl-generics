-- | Typeclass and an instance generator for the typeclass to convert
-- plain Haskell records to their vinyl representation.
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Vinyl.Generics.Transform where

import qualified Data.Promotion.Prelude.List   as L
import qualified Data.Promotion.Prelude.Show   as S
import qualified Data.Singletons.Prelude.Tuple as T
import           Data.Vinyl
import qualified Data.Vinyl.Functor            as VF
import           Data.Vinyl.Generics.Helpers   (fNamesTypeLevel)
import           Generics.SOP
import qualified Generics.SOP.NP               as GSN
import qualified GHC.TypeLits                  as TL
import           GHC.TypeLits                  
import           Language.Haskell.TH

-- | Type family that generates the column types for the vinyl representation.
type family ZipTypes (ns :: [Symbol])  (ys :: [*]) = (zs :: [(Symbol, *)]) | zs -> ns ys
type instance ZipTypes '[] '[] = '[]
type instance ZipTypes (n ': ns) (y ': ys)  =  '( n, y) ': (ZipTypes ns  ys)

-- | Typeclass for converting a plain Haskell record to it's vinyl
-- representation.
class ToVinyl a names rs | a -> names rs where
  type FieldNames a :: [Symbol]
  -- type FieldNames' a :: [*]
  toRecElField :: a  -> Rec VF.ElField (ZipTypes names rs)

-- | Helps generate an instance for @ToVinyl@, given a plain
-- Haskell record declaration name. 
deriveVinyl :: Name -> DecsQ
deriveVinyl name = entireInstance
  where
    n = conT name
    typeList1 = fNamesTypeLevel name
    entireInstance=
      [d|
        instance (((Code ($(n) f)) ~ '[rs]),
          (ns3 ~ FieldNames ($(n) f)) )
          => ToVinyl ($(n) f) ns3 rs where
          type FieldNames ($(n) f) = $(typeList1)
          -- type FieldNames' ($(n) f) = $(typeList1)
          toRecElField r = withNames $ go transformedNP
            where
              SOP (Z prod) = from r
              transformedNP = GSN.fromI_NP prod
              go = GSN.cata_NP RNil (:&)
         |]

type family MyEq (xs :: [Symbol])  (ys :: [Symbol]) (prefix :: Symbol) = (zs :: [Ordering])
type instance MyEq '[] '[] prefix = '[]
type instance MyEq (x ': xs) (y ': ys)  prefix =  (TL.CmpSymbol x (prefix S.<> y))  ': (MyEq xs  ys prefix)


class FromVinyl rs a where
  toPlainRecord :: Rec VF.ElField rs -> a

-- | Helps generate an instance for @FromVinyl@, given a prefix string for 
-- for the target record's fields
derivePlain :: String -> DecsQ
derivePlain fieldPrefixTarget = entireInstance
  where
    fieldPrefixTarget' = (litT . strTyLit) fieldPrefixTarget
    entireInstance =
      [d|
        instance (Generic a, StripFieldNames rs,
          (Code a ~ '[rs']), (Unlabeled rs) ~ rs',
          ((T.Fst (L.Unzip rs)) ~ rs1), 
          (intermed ~ (MyEq (FieldNames a) rs1 $(fieldPrefixTarget')),
          (intermed ~ L.Replicate (L.Length intermed) 'EQ))) => FromVinyl rs a where
          toPlainRecord vinylRec = to (SOP (Z $ helper (stripNames vinylRec)))
            where
              helper :: Rec VF.Identity rs -> NP I rs
              helper RNil                      = Nil
              helper ((VF.Identity x) :& rest) = (I x) :* (helper rest)
        |]

