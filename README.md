BrainFuck Haskell Implementation
========

Haskell implementation of BrainFuck language.

[BrainFuck wiki](https://en.wikipedia.org/wiki/Brainfuck)



Run BrainFuck in docker

```
  docker run --rm -v "$PWD/bf-src":/src -w /src icymint/brainfuck -f hanoi.b
```

Or use Shell script given in this repo

```
  ./brainfuck.sh -f hanoi.b
```
