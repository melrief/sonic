package sonic.example

import sonic._
import sonic.syntax._
import cats.syntax.flatMap._

object Main {

  def propReverse =
    Property(
      forAll(Gen.list(Range.linear(0, 100))(Gen.alpha)) >>=
      (list => list.reverse.reverse === list)
    )

  def listPropertyGroups =
    PropertyGroup("example")(
      "propReverse" -> propReverse
    )

  def main(args: Array[String]): Unit = {
    checkGroup(listPropertyGroups)
  }

}
