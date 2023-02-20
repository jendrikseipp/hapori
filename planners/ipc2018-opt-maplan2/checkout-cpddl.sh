#!/bin/bash

if [ ! -d cpddl-ch ]; then
    git clone git@github.com:danfis/cpddl-dev.git cpddl-ch
fi

cd cpddl-ch
git clean -fdx
if [ "$1" != "" ]; then
    git checkout $1
fi
git pull
git clean -fdx
cd ..

rm -rf cpddl/* cpddl/.[a-zA-Z0-9]*
cp -r cpddl-ch/{Makefile*,pddl,src,BSD-LICENSE} cpddl/
cp -r cpddl-ch/{.gitignore,.objs} cpddl/
