# panrec

`panrec` is a small CLI with big dreams. It's designed around the idea that

> all record types are the same,

whether that's a Scala `case class`, and Python `class`, and Haskell data type,
a Typescript `class`, or whatever you want. It's organized around the idea that
if you tell me you have a datamodel written in Scala and that there's a file
with case classes in it, I should be able to provide you with a file containing
Typescript or python or whatever classes that express the same constraints.

Currently the implementation aims for "`case classes` and `classes` in Typescript
are the same," which I understand is a bit of a walk back from "all record
types," but time exists, so please give me a minute.

It's inspired by the wonder [`pandoc`](https://github.com/jgm/pandoc).

Work up to this point on `panrec` was completed as part of a 10% time at Azavea.

# Getting started

## If you just want to use it

- [install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- clone the repo
- `stack install`
- `panrec --help`
- You can use either of these as an example to prove functionality for yourself:

```scala
package com.jisantuc.panrec

case class Foo(x: Int, y: IO[Option[Either[IO[Int], IO[String]]]])

object Foo {
  case class Bar(x: String)
}
```

```typescript
class Foo {
  x: number;
  y: Promise<Option<Either<Promise<number>, Promise<string>>>>;
  constructor(
    x: number,
    y: Promise<Option<Either<Promise<number>, Promise<string>>>>
  ) {
    this.x = x;
    this.y = y;
  }
}

class Bar {
  x: string;
  constructor(x: string) {
    this.x = x;
  }
}
```

## If you want to develop

The bulk of the work in the application runs through `attoparsec` and a single
typeclass called `Record`. `Record`s have a lot on them, and you only get one
thing for free, which is the `cruft` parser.

Workflow for adding a new record type would be something like:

- open `Python.hs` in your favorite editor
- create `data Python = Class { _fields :: [(String, Primitive)], _name :: String }`
- add a `Record` typeclass for your `Class` data type
- `stack build --file-watch`, and keep going until the compiler is happy
- add some tests of Python `Class`es that you'd expect to parse correctly
