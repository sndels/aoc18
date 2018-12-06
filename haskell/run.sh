#!/bin/bash
if [ ! -d bin ]; then
    mkdir bin
fi 
ghc -isrc -odir bin -hidir bin -o bin/aoc18 src/Main && ./bin/aoc18 $1 $2
