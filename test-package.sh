#!/bin/sh
set -e # Bail on first error
set -v # Be verbose

cd "$1"

# Install dependencies, build and run test suite
cabal-1.18 install   --enable-tests --only-dependencies
cabal-1.18 configure --enable-tests -v2  # -v2 provides useful information for debugging
cabal-1.18 build
cabal-1.18 test

# Check that tarball could be built and installed
cabal-1.18 sdist
SRC_TGZ=$(cabal-1.18 info . | awk '{print $2 ".tar.gz";exit}') ;
cd dist/
if [ -f "$SRC_TGZ" ]; then
    cabal-1.18 install "$SRC_TGZ"
else
    echo "expected '$SRC_TGZ' not found"
    exit 1
fi
