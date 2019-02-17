# vinyl-generics

[![Build Status](https://travis-ci.org/VinylRecords/vinyl-generics.png)](https://travis-ci.org/VinylRecords/vinyl-generics)

Convert plain Haskell records to [vinyl](https://hackage.haskell.org/package/vinyl) and vice versa.

## Potential Use Cases
* Reading an external data source (database query, results of API requests etc.) as a list of plain Haskell records and converting it to a list of `vinyl` records, and subsequently to a [data-frame](https://hackage.haskell.org/package/Frames) for exploratory analysis.
* Serializing a `data-frame`/list of `vinyl` records to JSON.
* Adding/removing fields from a plain record using `vinyl` as an intermediate representation.

## Usage
See `test/LibSpec.hs` for usage examples.
