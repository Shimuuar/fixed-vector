#!/bin/sh

cat ./dist-newstyle/build/x86_64-linux/*/*/build/FOO{4,8,16}.dump-timings | \
    sed -r '/WriteIface/d; s/FOS \{.*\}//; /(Simplifier|Float out)/!d'
