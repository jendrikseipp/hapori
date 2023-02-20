#!/bin/bash

set -e

if [ "$CPLEX" = "" ]; then
    CPLEX=/opt/cplex1271/cplex
fi

make -C boruvka clean
make -C opts clean
make -C cpddl clean
make clean
make -C bin clean

make CPLEX_CFLAGS=-I${CPLEX}/include \
       -C boruvka libboruvka.a

make BORUVKA_CFLAGS=-I../boruvka \
     BORUVKA_LDFLAGS="-L../boruvka -lboruvka" \
         -C cpddl

make -C opts

make CPLEX_CFLAGS=-I${CPLEX}/include \
     BORUVKA_CFLAGS="-Iboruvka -Icpddl" \
     OPTS_CFLAGS=-Iopts \

make CPLEX_CFLAGS=-I${CPLEX}/include \
     CPLEX_LDFLAGS="-L${CPLEX}/lib/x86-64_linux/static_pic -lcplex" \
	 BORUVKA_CFLAGS=-I../boruvka \
	 BORUVKA_LDFLAGS="-L../boruvka -lboruvka -L../cpddl -lpddl" \
	 OPTS_CFLAGS=-I../opts \
	 OPTS_LDFLAGS="-L../opts -lopts" \
	 	-C bin
