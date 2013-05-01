#!/bin/sh

run() {
    touch "$1"
    ghc -O2 -ddump-simpl -dsuppress-coercions "$1" > "$1.hcr"
    # ---
    printf "%-20s" "$1"
    grep -c writeDoubleArray "$1.hcr"
}

run fuse-simple.hs
run fuse-zipWith.hs
run fuse-mk.hs
run fuse-mapM.hs