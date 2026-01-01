2.0.1.0 [XXX]
-------------
* Support for GHC<9.2 dropped.
* `Prim` could be derived using `ViaFixed` by deriving via mechanism and add
  data types defined in library now has `Prim` instance.
* `Foldable1` could be derived using `ViaFixed`. All types for which it could be
  defined now has it. For GHC<9.6 `foldable1-classes-compat` is used.
* `ifoldl'` added.

2.0.0.0 [2025.07.10]
------------------
* Type family `Dim` returns Peano numbers instead of standard type level
  naturals.

  - `Index` type class restored and all indexing operation are performed in
  - `Arity` simplified
  - `CVecPeano` dropped and `ContVec` is parameterized using Peano numbers.

* In `ArityPeano` type class methods `reverseF` and `gunfoldF` are replaced
  with more general `accumPeano` and `reducePeano`.

* `Unbox` vector are fully reworked. All uses of data types with `Unbox`
  instances which are defined in the library except `Bool` should work without
  changes.

* `Data.Vector.Fixed.Cont.arity` dropped.

* Type of `D.V.F.Cont.withFun` generalized.

* Type class `VectorN` dropped. Use `QuantifiedConstraints` instead.

* Show instance now has form `[...]` instead of `fromList [...]`.

* `ViaFixed` newtype wrapper for deriving instances is
  added. `StorableViaFixed` is removed.

* `Data.Vector.Fixed.Storable.unsafeWith` ensures that pointer won't
  get GC'd while function runs.

* `Data.Vector.Fixed.sequenceA` is deprecated in favor of `sequence`.

* `foldl'` and `ifoldl'` functions added.

* Implement `sum` as in terms of `foldl'`.


1.2.3.0 [2023-10-31]
--------------------
* Pattern `V1` added
* `COMPLETE` pragmas added for patterns `V1`,`V2`,`V3`,`V4`


1.2.2.1 [2022-12-29]
--------------------
* Newtype `StorableViaFixed` for deriving `Storable` instances added.


1.2.1.1 [2022-12-26]
--------------------
* Fixed bug in `any` (#18)


1.2.1.0 [2021-11-13]
--------------------
* Support for GHC7.10 dropped.
* Pattern synonyms `V2`,`V3`,`V4` added.
* `replicate{,M}` and `generate{,M}` added.
* Functions `mk6`, `mk7`, `mk8` added.


1.2.0.0 [2018-09-02]
--------------------
* `Show` instance for data type now respect precedence.


1.1.0.0 [2018-03-11]
--------------------
* GHC8.4 compatibility release. Semigroup instances added and
  semigroup dependency added for GHC7.10


1.0.0.0 [2017-11-06]
--------------------
* Vector length now expressed as GHC's type level literals. Underlying
  implementation still uses Peano numbers to perform induction. This doesn't
  change user facing API much. Notably `FlexibleInstances` and
  `GADTs`/`TypeFamiles` are now required to write `Arity` constraint.
* `Monad` constraint is relaxed to `Applicative` where applicable. Duplicate
  functions are removed (`sequence` & `sequenceA` → `sequence`, etc)
* Module `Data.Vector.Fixed.Monomorphic` is dropped.
* Construction of N-ary vectors reworked. `Make` type class is gone.
* Boxed arrays now use SmallArrays internally.
* `overlaps` is removed from API for mutable vectors.
* `Data.Vector.Fixed.defaultRnf` is added.
* `Data.Vector.Fixed.Mutable.lengthI` is dropped.


0.9.0.0 [2016-09-14]
--------------------
* Simplification of `Arity` type class. This change shouldn't affect client
  code.
* Support for GHC < 7.8 is droppped.
* Fixed bug in `any`.


0.8.1.0 [2015-08-27]
--------------------
* `find` function added.


0.8.0.0 [2015-04-06]
--------------------
* NFData instances for all data type.
* Storable instances for all data types and default implementation of
  Storable's methods added.
* {i,}zipWith3 and {i,}zipWithM_ added.


0.7.0.3 [2015-01-03]
--------------------
* GHC 7.10 support


0.7.0.0 [2014-08-15]
--------------------
* Type level addition for unary numbers added
* `concat` function added
* More consistent naming for functions for working with `Fun`


0.6.4.0 [2014-04-15]
--------------------
* Isomorphism between Peano numbers and Nat added. (GHC >= 7.8)


0.6.3.1 [2014-03-12]
--------------------
* Documentation fixes.


0.6.3.0 [2014-02-22]
--------------------
* Left scans added.


0.6.2.0 [2014-02-07]
--------------------
* `Vec1` type synonym for boxed/unboxed/etc. vectors added.
* Vector instance for Data.Typeable.Proxy (GHC >= 7.8)


0.6.1.1 [2014-02-04]
--------------------
* GHC 7.8 support


0.6.1.0 [2014-01-24]
--------------------
* `distribute` `collect` and their monadic variants added.


0.6.0.0 [2013-11-17]
--------------------
* Data instance for all array-based vectors added.
* Storable instance added for `Storable.Vec`.
* Monoid instances added for all vectors.

0.5.1.0 [2013-08-06]
--------------------
* Zero-element vector `Empty'`is added.


0.5.0.0 [2013-08-02]
--------------------
* `ContVec` now behaves like normal vector. `Arity` type class is
  reworked. `Id` data type is removed.
* Construction of vector reworked.
* `reverse`, `snoc`, `consV`, `fold` and `foldMap` are added.
* Type changing maps and zips are added.
* Vector indexing with type level numbers is added.
* Twan van Laarhoven's lens added. (`element` and `elementTy`)
* Ord instances added to vector data types defined in the library.


0.4.4.0 [2013-06-13]
--------------------
* Functor and Applicative instances are added to Id.


0.4.3.0 [2013-05-18]
--------------------
* Typeable instance for S and Z added.


0.4.2.0 [2013-05-01]
--------------------
* 1-tuple `Only` added.
* `fromList'` and fromListM added.
* apply functions from Arity type class generalized.


0.4.1.0 [2013-04-29]
--------------------
* `cons` function added.
* Getter for `Fun` data type added.


0.4.0.0 [2013-04-04]
--------------------
* Wrapper for monomorphics vectors is added.
* `VecList` is reimplemented as GADT and constructors are exported.
* Constructor of `ContVecT` is exported
* Empty `ContVecT` is implemented as `empty`.
* Typeable, Foldable and Traversable instances are added where
  appropriate


0.3.0.0 [2013-03-06]
--------------------
* Vector type class definition is moved to the D.V.F.Cont module.
* Indexing function restored.
* `unfoldr` added.


0.2.0.0 [2013-02-10]
--------------------
* Continuation-based vector added.
* Right fold added.
* tailWith, convertContinuation, and ! from
  Data.Vector.Fixed removed.
* Vector instance for tuples added.


0.1.2 [2013-01-26]
------------------
* imap, imapM, ifoldl, ifoldM, zipWithM, izipWithM
  functions are added.
* VectorN type class added.


0.1.1 [2012-11-29]
------------------
* foldM and tailWith added. Type synonyms for numbers up to 6 are
  added. Fun is reexported from Data.Vector.Fixed.
