#!/bin/sh
cat \
  head \
  ../preface.txt sep \
  ../setting-world.txt sep \
  ../setting-mainchar.txt sep \
  ../setting-mainchar-desperate.txt sep \
  ../setting-leave-by-westgate.txt sep \
  tail \
  > combined.tex
