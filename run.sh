#!/bin/bash

echo "Extracting and evaluating cost models"

for i in `find examples -iname '*.hs'`; do
  echo
  echo "Evaluating file \'$i\'"
  echo
  stack runhaskell $i
  echo "Done, press any key to continue"
  read
done
