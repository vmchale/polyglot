#!/usr/bin/env bash

# prolegomena
set -e
set pipefail

# set our searcher
if [ -x rg ]; then
  grepcmd="rg"
else
  grepcmd="grep"
fi

# check for existence of 'shake' package
function find_shake {
  ghc-pkg list | $grepcmd 'shake-ext-0.3.0.0'
}

PATH=$HOME/.cabal/bin:$PATH

if [ ! "$(ls "$HOME"/.cabal/bin/happy)" ]; then
  cabal update
  cabal install happy
fi

if [ ! "$(ls "$HOME"/.cabal/bin/happy)" ]; then
  cabal update
  cabal install alex
fi

if [ ! "$(find_shake)" ]; then
  cabal update
  cabal install shake-ext
fi

mkdir -p .shake
cd .shake
cp ../shake.hs .
ghc-8.2.2 -O shake.hs -o ../build -Wall -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates
rm shake.hs
