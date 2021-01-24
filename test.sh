#!/bin/bash
set -e

for x in 0 1 2 3; do
  sed \
    -e 's/toggle \(.\)\(.\)/toggle \2\1/g' \
    -i.bak "src/Test$x.hs" 
done

if stack build 2> out; then
  cat out | \
    tr '\n' '\1' | \
    sed 's/\[Test\([0-9]\)*\]:Result size of \([^]*\)  = {terms: [0-9]*, types: [0-9]*, coercions: \([0-9]*\)/OUT: \1: \2: \3/g' | \
    tr '\1' '\n' | \
    grep '^OUT' | \
    grep 'Desugar (before optimization)' | \
    sed 's/^OUT: \([0-9]*\): [^:]*: \([0-9]*\)$/\1: \2 coercions/g'
else
  cat out 1>&2
fi
