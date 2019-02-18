# vinyl-generics

[![Build Status](https://travis-ci.org/VinylRecords/vinyl-generics.png)](https://travis-ci.org/VinylRecords/vinyl-generics)

Convert plain Haskell records to [vinyl](https://hackage.haskell.org/package/vinyl) and vice versa, via `GHC.Generics` and `generics-sop`/`records-sop`.

## Potential Use Cases
* Reading an external data source (database query, results of API requests etc.) as a list of plain Haskell records and converting it to a list of `vinyl` records (for subsequent conversion to an in-memory [data-frame](https://hackage.haskell.org/package/Frames)).
* Serializing a `Frame`/list of `vinyl` records to JSON.
* Adding/removing fields from a plain record using `vinyl` as an intermediate representation.

## Usage
Consider the following example module: 

```haskell 
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Example where

import           Data.Aeson
import           Data.Text
import           Data.Vinyl
import           Data.Vinyl.Generics.Transform (fromVinyl, toVinyl)
import qualified Generics.SOP                  as S
import qualified GHC.Generics                  as G

data MyPlainRecord = MPR {
  age      :: Int,
  iscool   :: Bool,
  yearbook :: Text
} deriving (Show, G.Generic)

instance S.Generic MyPlainRecord
instance S.HasDatatypeInfo MyPlainRecord

data MyType = 
  MyType { 
    bike :: Bool
  , skateboard :: Bool 
  } deriving (Show, G.Generic)

data MyPlainRecord2 = MPR2 {
  age      :: Int,
  iscool   :: Bool,
  yearbook :: Text,
  hobbies  :: MyType
} deriving (Show, G.Generic)

instance S.Generic MyPlainRecord2
instance S.HasDatatypeInfo MyPlainRecord2

```

In the above, let `MyPlainRecord` be the format in which data is being read from an external source. We also read some additional data `additionalFields` (say from a CSV using `Frames`): 

```haskell
-- some mock data
r1 :: MyPlainRecord
r1 = MPR { age = 23, iscool = True, yearbook = "!123!"}

additionalFields :: Rec ElField '[("age" ::: Int), ("hobbies" ::: MyType)]
additionalFields = xrec (23, MyType { bike = True, skateboard = True})
```


We'd like to add the field `hobbies :: MyType` to one such record (` ::  MyPlainRecord`), and want to have a record of type `MyPlainRecord2`. We can accomplish this by first isolating the field:
```haskell
getHobbies :: Rec ElField '[("age" ::: Int), ("hobbies" ::: MyType)] 
             -> Rec ElField '[("hobbies" ::: MyType)]
getHobbies = rcast
```

...and then appending it to the `vinyl` representation of the plain record:

```haskell
go :: MyPlainRecord2
go = fromVinyl $ (toVinyl r1) `rappend` (getHobbies additionalFields)
```

That's all there is to it. Once we have our vinyl record in plain record form, it is straightforward to serialize it to JSON: 

```haskell
instance ToJSON MyPlainRecord2
instance ToJSON MyType
```

Have a look at `test/LibSpec.hs` for more usage examples.

## Known Limitations
This library in it's current form works only with `vinyl` records with type-level field names (i.e. use the `ElField` interpretation functor).
Future versions hope to tackle records with anonymous fields (e.g. heterogenous lists making use of the `Identity` functor) as well.

