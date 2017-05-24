#!/bin/sh

rm -r z3*
if [ ! -d ./z3 ]; then

    URL="https://github.com/Z3Prover/z3/releases/download/z3-4.5.0/"
    FORMAT=".zip"

    if [ `uname` = "Linux" ]; then
        echo "getting z3 for " `uname`
        EXECUTABLE="z3-4.5.0-x64-ubuntu-14.04"


    elif [ `uname` = "Darwin" ]; then
        echo "getting z3 for " `uname`
        EXECUTABLE="z3-4.5.0-x64-osx-10.11.6"
    fi

    curl -OL $URL$EXECUTABLE$FORMAT && tar -xvf $EXECUTABLE$FORMAT && mv $EXECUTABLE"/" z3 && rm $EXECUTABLE$FORMAT

fi