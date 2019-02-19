-- | Generic functions to convert plain Haskell records
--  to their @vinyl@ representation and back.
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

-- | This typeclass provides a method to change the
-- interpretation functor of a particular @vinyl@ record
-- from @P@ to @ElField@.
class NatTrans rs where
  rmap' :: Rec SR.P rs -> Rec ElField rs

instance NatTrans '[] where
  rmap' RNil = RNil

instance (NatTrans xs, x ~ '(s, t), KnownSymbol s) => NatTrans (x ': xs) where
  rmap' ((SR.P x) :& xs) =  (Field x) :& rmap' xs

-- | This typeclass constrains that given a type-level list of field-name,
-- field-type tuples (i.e. @rs@), it is possible to "fold" in general sense
-- (i.e. a "catamorphism") over a @vinyl@ record parameterised by @rs@,
-- swap all it's constructors with those from @NP@ (N-ary product from
-- @generics-sop@) and discard the field names by using the @I@
-- interpretation functor from @generics-sop@ and yield something
-- of type @NP I ys@.
class Cata rs ys | rs -> ys where
  recToNP :: Rec ElField rs -> NP I ys

instance Cata '[] '[] where
  recToNP RNil = Nil

instance (Cata xs ys, y ~ SR.Snd x) => Cata (x ': xs) (y ': ys) where
  recToNP ((Field x) :& xs) =  (I x) :* recToNP xs

-- | Given a plain record, returns the @vinyl@ equivalent with the
-- field names as type-level strings, tupled with the field type.
--
-- Example:
--
-- @
--    import qualified Generics.SOP   as S
--    import qualified GHC.Generics   as G
--
--    data MyPlainRecord = MPR {
--        age      :: Int,
--        iscool   :: Bool,
--        yearbook :: Text
--      } deriving (Show, G.Generic)
--
--    instance S.Generic MyPlainRecord
--    instance S.HasDatatypeInfo MyPlainRecord
--    -- Note: requires all 3 instances: G.Generic, S.Generic and S.HasDatatypeInfo
--
--    -- this now works! Type signature is optional here.
--    convertToVinyl :: MyPlainRecord
--                   -> Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
--    convertToVinyl  = toVinyl
-- @
--
toVinyl :: (SR.IsRecord a rs, NatTrans rs) => a -> Rec ElField rs
toVinyl r = rmap' (cata_NP RNil (:&) np)
    where
      np = SR.toRecord r

-- | Given a @vinyl@ record, returns the plain record equivalent.
-- Requires the equivalent plain record data declaration to be available
-- in current scope.
--
-- Additionally, it requires explicit type annotation (either using a
-- type signature or using @TypeApplications@).
--
-- Example:
--
-- @
--    import qualified Generics.SOP   as S
--    import qualified GHC.Generics   as G
--
--    r1 :: Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
--    r1 = xrec (23, True, "!123!")
--
--    data MyPlainRecord = MPR {
--        age      :: Int,
--        iscool   :: Bool,
--        yearbook :: Text
--      } deriving (Show, G.Generic)
--
--    instance S.Generic MyPlainRecord
--    -- Note: Here we need only G.Generic and S.Generic
--
--    -- Using explicit type signature
--    r2 :: MyPlainRecord
--    r2 = fromVinyl r1
--
--    -- or using TypeApplications
--    r2' = fromVinyl @MyPlainRecord r1
-- @
--
fromVinyl :: (Generic a,  Code a ~ ts, ts ~ '[ys], Cata rs ys)
          => Rec ElField rs
          -> a
fromVinyl = to . SOP . Z . recToNP
