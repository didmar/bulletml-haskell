# What is BulletML ?

BulletML is a language that describe bullet patterns in shooting games. It was created by Kenta Cho ([his website](http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/index_e.html))

# BulletML in Haskell

This is a library to parse BulletML files. I made it a way to introduce myself to XML parsing in Haskell with [HXT](https://hackage.haskell.org/package/hxt).
Note that this is a work in progress !

# How to install and test

cabal sandbox init
cabal install --enable-tests --dependencies-only
cabal exec -- runhaskell -isrc -itest test/TestBulletML.hs
