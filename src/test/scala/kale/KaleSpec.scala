package kale

import kale.LOGIC.Variable
import org.scalatest.FreeSpec

class KaleSpec extends FreeSpec {

  val X = Variable("X")

  import Implicits._

  "SET" in {
    val set = new SET("_,_", 0)
    assert(set.op(1, 2) == set.op(2, 1))
    assert(set.op(0, 2) == (2: Term))
  }

  "INT" - {
    import INT._

    val x: Token[Int] = implicitly[Token[Int]](2)
    val y: Token[Int] = implicitly[Token[Int]](3)

    assert(x == Int(2))
    assert(x != y)

    "Int" in {
      assert(x.value == 2)
      assert(x.label == INT.Int)
    }
    "+" in {
      assert(x + y == Int(5))
      assert(x + X != Int(5))
      assert(x + 0 == Int(2))
      assert(x + 8 + 0 + y + X + 5 == (13: Term) + X + 5)
    }
  }
}
