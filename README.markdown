
# Fixed-vector [![Build Status](https://travis-ci.org/Shimuuar/fixed-vector.png?branch=master)](https://travis-ci.org/Shimuuar/fixed-vector)

Generic library for vectors with statically known size. Same functions could be
used to work with both ADT based vector like 

```haskell
data Vec3 a = Vec3 a a a
```

Tuples are vectors too:

```
>>> sum (1,2,3)
6
```

Vectors which are represented internally by arrays are provided by library. Both
boxed and unboxed arrays are supported.


# Get involved!

Please report bugs via
[github issue tracker](https://github.com/Shimuuar/fixed-vector/issues)

You can create and contribute patches using either mercurial or git:

[git mirror](https://github.com/Shimuuar/fixed-vector)

* `git clone git://https://github.com/Shimuuar/fixed-vector.git`

[mercurial mirror](https://bitbucket.org/Shimuuar/fixed-vector)

* `hg clone https://bitbucket.org/Shimuuar/fixed-vector`


# Authors

Library is written and maintained by Aleksey Khudyakov <alexey.skladnoy@gmail.com>
