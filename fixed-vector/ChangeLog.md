Changes in 1.0.0.0

  * GHC8.4 compatibility release. Semigroup instances added and
    semigroup dependency added for GHC7.10

Changes in 1.0.0.0

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

Changes in 0.9.0.0

  * Simplification of `Arity` type class. This change shouldn't affect client
    code.

  * Support for GHC < 7.8 is droppped.

  * Fixed bug in `any`.


Changes in 0.8.1.0

  * `find` function added.


Changes in 0.8.0.0

  * NFData instances for all data type.

  * Storable instances for all data types and default implementation of
    Storable's methods added.

  * {i,}zipWith3 and {i,}zipWithM_ added.


Changes in 0.7.0.3

  * GHC 7.10 support


Changes in 0.7.0.0

  * Type level addition for unary numbers added

  * `concat` function added

  * More consistent naming for functions for working with `Fun`


Changes in 0.6.4.0

  * Isomorphism between Peano numbers and Nat added. (GHC >= 7.8)


Changes in 0.6.3.1

  * Documentation fixes.


Changes in 0.6.3.0

  * Left scans added.


Changes in 0.6.2.0

  * `Vec1` type synonym for boxed/unboxed/etc. vectors added.

  * Vector instance for Data.Typeable.Proxy (GHC >= 7.8)


Changes in 0.6.1.1

  * GHC 7.8 support


Changes in 0.6.1.0

  * `distribute` `collect` and their monadic variants added.


Changes in 0.6.0.0

  * Data instance for all array-based vectors added.

  * Storable instance added for `Storable.Vec`.

  * Monoid instances added for all vectors.


Changes in 0.5.1.0

  * Zero-element vector `Empty'`is added.


Changes in 0.5.0.0

  * `ContVec` now behaves like normal vector. `Arity` type class is
    reworked. `Id' data type is removed.

  * Construction of vector reworked.

  * `reverse`, `snoc`, `consV`, `fold` and `foldMap` are added.

  * Type changing maps and zips are added.

  * Vector indexing with type level numbers is added.

  * Twan van Laarhoven's lens added. (`element` and `elementTy`)

  * Ord instances added to vector data types defined in the library.


Changes in 0.4.4.0

  * Functor and Applicative instances are added to Id.


Changes in 0.4.3.0

  * Typeable instance for S and Z added.


Changes in 0.4.2.0

  * 1-tuple `Only` added.

  * `fromList'` and fromListM added.

  * apply functions from Arity type class generalized.


Changes in 0.4.1.0

  * `cons` function added.

  * Getter for `Fun` data type added.


Changes in 0.4.0.0

  * Wrapper for monomorphics vectors is added.

  * `VecList` is reimplemented as GADT and constructors are exported.

  * Constructor of `ContVecT` is exported

  * Empty `ContVecT` is implemented as `empty`.

  * Typeable, Foldable and Traversable instances are added where
    appropriate


Changes in 0.3.0.0

  * Vector type class definition is moved to the D.V.F.Cont module.

  * Indexing function restored.

  * `unfoldr` added.


Changes in 0.2.0.0

  * Continuation-based vector added.

  * Right fold added.

  * tailWith, convertContinuation, and ! from
    Data.Vector.Fixed removed.

  * Vector instance for tuples added.


Changes in 0.1.2

  * imap, imapM, ifoldl, ifoldM, zipWithM, izipWithM
    functions are added.

  * VectorN type class added.


Changes in 0.1.1

  * foldM and tailWith added. Type synonyms for numbers up to 6 are
    added. Fun is reexported from Data.Vector.Fixed.
