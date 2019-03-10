#!/bin/sh
cd "${0%/*}"
set -ex

./tests/test-ndarray.sps
./bin/ndarray.sps

echo All tests passed
