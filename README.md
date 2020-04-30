# availer

A Haskell library to manage intervals.

It allows you to consider intervals with included and excluded boundaries.
It provides functions to compute the intersection and the union of intervals. Moreover, it allows to check the relative position of two intervals, according to [Allen's interval algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra).

## build

The library is built using [stack](http://haskellstack.org/) and can be built using

```
stack build
```

## test

The library is tested using [hspec](https://hackage.haskell.org/package/hspec) and [QuickCheck](https://hackage.haskell.org/package/QuickCheck). The tests can be run using

```
stack test
```

## documentation

You can build the library documentation using

```
stack haddock
```
