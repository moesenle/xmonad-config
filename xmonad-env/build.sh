#!/bin/sh

nix-shell --pure --command 'ghc --make ../xmonad.hs -i -ilib -fforce-recomp -v0 -o ../xmonad-x86_64-linux'
