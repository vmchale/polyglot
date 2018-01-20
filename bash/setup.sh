#!/usr/bin/env bash

# prolegomena
set -e
set pipefail

# check for existence of 'shake' package
if [ -x rg ]; then
  grepcmd="rg"
else
  grepcmd="grep"
fi

function find_shake {
  ghc-pkg list | $grepcmd 'shake-ext'
}

if [ ! $(find_shake) ]; then
  cabal update
  cabal install shake-ext
fi

mkdir -p .shake
cd .shake
cp ../shake.hs .
ghc-8.2.2 -O shake.hs -o ../build -Wall -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates
