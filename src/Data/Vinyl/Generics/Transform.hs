-- | Typeclass and an instance generator for the typeclass to convert
-- plain Haskell records to their vinyl representation.
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Vinyl.Generics.Transform(
  toVinyl,
  fromVinyl
) where

import           Data.Vinyl
import           Generics.SOP
import           Generics.SOP.NP
import qualified Generics.SOP.Record as SR
import           GHC.TypeLits

class RMap' rs where
  rmap' :: Rec SR.P rs -> Rec ElField rs

instance RMap' '[] where
  rmap' RNil = RNil

instance (RMap' xs, x ~ '(s, t), KnownSymbol s) => RMap' (x ': xs) where
  rmap' ((SR.P x) :& xs) =  (Field x) :& rmap' xs


class RMap'' rs ys | rs -> ys where
  rmap'' :: Rec ElField rs -> NP I ys

instance RMap'' '[] '[] where
  rmap'' RNil = Nil

instance (RMap'' xs ys, y ~ SR.Snd x) => RMap'' (x ': xs) (y ': ys) where
  rmap'' ((Field x) :& xs) =  (I x) :* rmap'' xs

toVinyl :: (SR.IsRecord a r, RMap' r) => a -> Rec ElField r
toVinyl r = rmap' (cata_NP RNil (:&) np)
    where
      np = SR.toRecord r

fromVinyl ::
  (Generic a,  Code a ~ ts,
  ts ~ '[ys], RMap'' rs ys) => Rec ElField rs -> a
fromVinyl = (to . SOP . Z . rmap'')
