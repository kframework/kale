package org.kframework.kale

import org.scalatest.FreeSpec

class KaleSpec extends FreeSpec {

  val X = Variable("X")

  import Implicits._

//  "SET" in {
//    val set = new SET("_,_", 0)
//    set.op(1, 2)
//
//    assert(set.op(1, 2) == set.op(2, 1))
//    assert(set.op(0, 2) == (2: Term))
//  }

  "INT" - {
    val x: Constant[Int] = 2
    val y: Constant[Int] = 3

    assert(x == INT(2))
    assert(x != y)

    "Int" in {
      assert(x.value == 2)
      assert(x.label == INT)
    }
//    "+" in {
//      assert(x + y == INT(5))
//      assert(x + X != INT(5))
//      assert(x + 0 == INT(2))
//      assert(x + 8 + 0 + y + X + 5 == (13: Term) + X + 5)
//    }
  }
}
