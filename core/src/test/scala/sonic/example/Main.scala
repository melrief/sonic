package sonic.example

import cats.syntax.all._

import sonic._
import sonic.syntax._

object Main {

  val exampleGroup =
    PropertyGroup("example")(
        "propSuccess0" ->
          Property(success)
      , "propSuccess1" ->
          Property(assert(true))
      , "propFailure0" ->
          Property(failure)
      , "propFailure1" ->
          Property(assert(false))
      , "propRange" ->
          Property(forAll(Gen.int(Range.constant(0, 2))) >>= (x => assert(x >= 0 && x <= 2)))
      , "propAlphaId" ->
          Property(forAll(Gen.alpha) >>= (x => x === x))
      , "propAlphaNumId" ->
          Property(forAll(Gen.alphaNum) >>= (x => x === x))
      , "propReverseConstant" ->
          Property(
            forAll(Gen.collection[List, Char](Range.constant(0, 20))(Gen.alphaNum)) >>=
            (list => list.reverse.reverse === list))
      , "propStrings" ->
          Property(
            forAll(Gen.string(Range.constant(0, 20))(Gen.lower)) >>=
            (str => str.toLowerCase() === str))
    )

  def main(args: Array[String]): Unit = {
    checkGroup(exampleGroup)
  }
}
