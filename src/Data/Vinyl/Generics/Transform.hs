-- | Polymorphic functions to convert plain Haskell records
--  to their vinyl representation and back.
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Vinyl.Generics.Transform(
    toVinyl
  , fromVinyl
) where

import           Data.Vinyl
import           Generics.SOP
import           Generics.SOP.NP
import qualified Generics.SOP.Record as SR
import           GHC.TypeLits

class NatTrans rs where
  rmap' :: Rec SR.P rs -> Rec ElField rs

instance NatTrans '[] where
  rmap' RNil = RNil

instance (NatTrans xs, x ~ '(s, t), KnownSymbol s) => NatTrans (x ': xs) where
  rmap' ((SR.P x) :& xs) =  (Field x) :& rmap' xs


class Cata rs ys | rs -> ys where
  recToNP :: Rec ElField rs -> NP I ys

instance Cata '[] '[] where
  recToNP RNil = Nil

instance (Cata xs ys, y ~ SR.Snd x) => Cata (x ': xs) (y ': ys) where
  recToNP ((Field x) :& xs) =  (I x) :* recToNP xs

toVinyl :: (SR.IsRecord a rs, NatTrans rs) => a -> Rec ElField rs
toVinyl r = rmap' (cata_NP RNil (:&) np)
    where
      np = SR.toRecord r

fromVinyl ::
  (Generic a,  Code a ~ ts,
  ts ~ '[ys], Cata rs ys) => Rec ElField rs -> a
fromVinyl = (to . SOP . Z . recToNP)
