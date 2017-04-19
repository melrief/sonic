# sonic

[![Build Status](https://travis-ci.org/melrief/sonic.svg?branch=master)](https://travis-ci.org/melrief/sonic.svg?branch=master)

Sonic is a Scala implementation of [hedgehog](https://github.com/hedgehogqa) using [Cats](https://github.com/typelevel/cats)
and [Monix](https://github.com/monix/monix). It is a direct translation of the
[haskell implementation](https://github.com/hedgehogqa/haskell-hedgehog). It's still in early stage and
lacks many parts of the original library but the core data structures are all implemented and working.

## Features

- Integrated shrinking, shrinks obey invariants by construction.
- Generators allow monadic effects.
- Range combinators for full control over the scope of generated numbers and collections.

## Example

You can find the following example in the subdirectory [example](example).

Import `sonic._`, `cats.syntax.flatMap._` and optionally `sonic.syntax._`.

```scala
import sonic._
import sonic.syntax._
import cats.syntax.flatMap._
```

Once you have your imports set up, you can write a simple property:

```scala
def propReverse =
    Property(
      forAll(Gen.list(Range.linear(0, 100))(Gen.alpha)) >>=
      (list => list.reverse.reverse === list)
    )
```

Then define a group containing the property that you have just defined:

```scala
def listPropertyGroups =
  PropertyGroup("example")(
    "propReverse" -> propReverse
  )
```

Finally, run the property group with `Runner.checkGroup`:

```scala
def main(args: Array[String]): Unit = {
  checkGroup(listPropertyGroups)
}
```

and run the Scala program:

```scala
$ sbt example/run
━━━ example ━━━
  propReverse: true
```

## Contributing

Contributions are welcomed in the form of PRs.

People are expected to follow the [Typelevel Code of Conduct](http://typelevel.org/conduct.html) when discussing sonic 
on the Github page, Gitter channel, or other venues.

## License

[MPL-2.0](https://www.mozilla.org/en-US/MPL/2.0/)
