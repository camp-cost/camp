#!/bin/bash

echo "Extracting and evaluating cost models"

for i in `find examples ! -name 'Examples.hs' -iname '*.hs'`; do
  echo
  echo "Evaluating file \'$i\'"
  echo
  stack runhaskell $i
  echo "Done, press ENTER to continue"
  read
done
