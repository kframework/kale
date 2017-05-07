#!/bin/sh

if [ ! -d ./z3 ]; then
    OSNAME=`uname`

    if [ "$OSNAME"="Linux" ]; then
        echo "Linux"
        # fix this to select Linux version
        URL="https://github.com/Z3Prover/z3/releases/download/z3-4.5.0/z3-4.5.0-x64-ubuntu-14.04.zip"
    fi

    if [ "$OSNAME"="Darwin" ]; then
        echo "Darwin"
        URL="https://github.com/Z3Prover/z3/releases/download/z3-4.5.0/z3-4.5.0-x64-osx-10.11.6.zip"
    fi

    mkdir z3
    curl -L $URL | tar -xf- -C z3 --strip-components=1
fi
