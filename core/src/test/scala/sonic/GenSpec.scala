package sonic

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import eu.timepit.refined.auto._

class GenSpec extends FlatSpec with Matchers with OptionValues
  with GenIntSpec

trait GenIntSpec { this: FlatSpec with Matchers with OptionValues =>

  behavior of "Gen.int"

  it should "generate integers in the given range and shrink towards the origin" in {
    val origin = 2000
    val gen = Gen.int(Range.constantFrom(origin, 1970, 2100))
    val tree = gen.run(0)(Seed.from(0L))
    tree.runTree.value.value shouldEqual 2039
    val shrinks = tree.runTree.value.children().map(_.runTree.value.value)
    shrinks.head shouldEqual origin // first shrink is always the origin
    for ((node, upBound) <- (shrinks.tail zip shrinks)) {
      node should (be < 2039 and be >= upBound)
    }
  }
}
