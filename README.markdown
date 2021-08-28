
# Fixed-vector [![Build Status](https://travis-ci.org/Shimuuar/fixed-vector.png?branch=master)](https://travis-ci.org/Shimuuar/fixed-vector)

Generic library for vectors with statically known size. It's able to work with
product types where types of all elements are same. For example following type
could be used:

```haskell
data Vec3 a = Vec3 a a a
```

Tuples of same types work as well:

```
>>> sum (1,2,3)
6
```

Library provides set of vector parametrized by length. Boxed, unboxed and
storable vectors are all supported.

Basic idea is to establish isomorphism between N-element vector and its Church
encoding (`∀r. (a → a → r) → r` for 2-element vector) and all functions work on
Church-encoded vectors. This allows to decouple functions from representation of
vectors and allows to implement deforestation.

Downside of this approach is inability to work with vectors larger than tens of
elements. If you need larger
vectors [vector-sized](https://hackage.haskell.org/package/vector-sized) could provide
similar functionality.


# Get involved!

Please report bugs via
[github issue tracker](https://github.com/Shimuuar/fixed-vector/issues)



# Authors

Library is written and maintained by Aleksey Khudyakov <alexey.skladnoy@gmail.com>
