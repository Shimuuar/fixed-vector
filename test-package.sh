#!/bin/sh
set -e # Bail on first error
set -v # Be verbose

cd "$1"

# Install dependencies, build and run test suite
cabal install   --enable-tests --only-dependencies
cabal configure --enable-tests -v2  # -v2 provides useful information for debugging
cabal build
cabal test

# Check that tarball could be built and installed
cabal sdist
SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
cd dist/
if [ -f "$SRC_TGZ" ]; then
    cabal install "$SRC_TGZ"
else
    echo "expected '$SRC_TGZ' not found"
    exit 1
fi
