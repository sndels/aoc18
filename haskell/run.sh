#!/bin/bash
if [ ! -d bin ]; then
    mkdir bin
fi
if [ $1 == "o2" ]; then
    ghc -O2 -isrc -odir bin -hidir bin -o bin/aoc18 src/Main && ./bin/aoc18 $2 $3
else
    ghc -isrc -odir bin -hidir bin -o bin/aoc18 src/Main && ./bin/aoc18 $1 $2
fi
