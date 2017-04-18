package sonic.shrink

import org.scalatest.{FlatSpec, Matchers}

class ShrinkSpec extends FlatSpec with Matchers
  with ShrinkListSpec
  with RemovesSpec
  with HalvesSpec
  with ConsNubSpec

trait ShrinkListSpec { this: FlatSpec with Matchers =>

  behavior of "shrinkList"

  it should "produce [[],[2,3],[1,3],[1,2]] from [1,2,3]" in {
    shrinkList(List(1,2,3)) should contain theSameElementsInOrderAs List(Nil,List(2,3),List(1,3),List(1,2))
  }

  it should """produce ["","cd","ab","bcd","acd","abd","abc"] from "abcd"""" in {
    shrinkList("abcd".toList) should contain theSameElementsInOrderAs List("","cd","ab","bcd","acd","abd","abc").map(_.toList)
  }
}

trait RemovesSpec { this: FlatSpec with Matchers =>

  behavior of "removes"

  it should """produce ["cdef","abef","abcd"] from 2 and "abcdef"""" in {
    removes(2, "abcdef".toList) should contain theSameElementsInOrderAs List("cdef","abef","abcd").map(_.toList)
  }
}

trait HalvesSpec { this: FlatSpec with Matchers =>

  behavior of "halves"

  it should "produce [15,7,3,1] from 15" in {
    halves(15) should contain theSameElementsInOrderAs List(15, 7, 3, 1)
  }

  it should "produce [100,50,25,12,6,3,1] from 100" in {
    halves(100) should contain theSameElementsInOrderAs List(100,50,25,12,6,3,1)
  }

  it should "produce [-26,-13,-6,-3,-1] from -26" in {
    halves(-26) should contain theSameElementsInOrderAs List(-26,-13,-6,-3,-1)
  }
}

trait ConsNubSpec { this: FlatSpec with Matchers =>

  behavior of "consNub"

  it should "add any element to an empty list" in {
    consNub(1, Nil) shouldEqual List(1)
  }

  it should "add an element x to a non-empty list l if l.head != x" in {
    consNub(1, List(2)) shouldEqual List(1, 2)
  }

  it should "not add an element x to a non-empty list l if l.head == x" in {
    consNub(1, List(1)) shouldEqual List(1)
  }
}
