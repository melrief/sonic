package sonic

import org.scalatest.{FlatSpec, Matchers}

import eu.timepit.refined.auto._

class RangeSpec extends FlatSpec with Matchers {

  "Range.singleton" should "build a constant value" in {
    val range = Range.singleton(5)
    range.origin shouldEqual 5
    range.bounds(0) shouldEqual (5 -> 5)
  }

  "Range.constant" should "build a range unaffected by the size" in {
    val range = Range.constant(0, 100)
    range.origin shouldEqual 0
    range.bounds(0) shouldEqual (0 -> 100)
  }

  "Range.constantFrom" should "build a range unaffected by the size" in {
    val range = Range.constantFrom(0, -10, 10)
    range.origin shouldEqual 0
    range.bounds(0) shouldEqual (-10 -> 10)
  }

  "Range.linear" should "build a range which scales linearly with the size" in {
    val range = Range.linear(0, 100)
    range.bounds(0) shouldEqual (0 -> 0)
    range.bounds(10) shouldEqual (0 -> 10)
    range.bounds(100) shouldEqual (0 -> 100)
  }

  "Range.linearFrom" should "build a range which scales linearly with the size" in {
    val range = Range.linearFrom(0, -10, 10)
    range.bounds(0) shouldEqual (0 -> 0)
    range.bounds(50) shouldEqual (-5 -> 5)
    range.bounds(100) shouldEqual (-10 -> 10)
  }

  "Range.clamp" should "truncate a value so it stays within some range" in {
    Range.clamp(5, 10, 15) shouldEqual 10
    Range.clamp(5, 10, 0) shouldEqual 5
  }
}
