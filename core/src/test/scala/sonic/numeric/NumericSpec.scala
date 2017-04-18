package sonic.numeric

import org.scalatest.{FlatSpec, Matchers}

class NumericSpec extends FlatSpec with Matchers with PopCountTest {

  popCountTests(
    0x0 -> 0,
    0x1 -> 1,
    0x2 -> 1,
    0x3 -> 2,
    0x4 -> 1,
    0x5 -> 2,
    0x6 -> 2,
    0x7 -> 3,
    0x8 -> 1,
    0x9 -> 2,
    0xA -> 2,
    0xB -> 3,
    0xC -> 2,
    0xD -> 3,
    0xE -> 3,
    0xF -> 4
  )

}

trait PopCountTest { self: FlatSpec with Matchers =>

  def popCountTest(input: Int, expectedCount: Int): Unit =
    it should s"""return $expectedCount for "$input"""" in { popCount(input) shouldBe expectedCount }

  def popCountTests(data: (Int, Int)*): Unit = {
    behavior of "NumericSpec.popCount"
    data foreach (popCountTest _).tupled
  }
}
