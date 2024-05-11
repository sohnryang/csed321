#!/bin/sh
eval $(opam env --switch=405)
rm typing.ml
git restore "typing.*"
make clean
make
cp typing_stub.ml typing.ml
