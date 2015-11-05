package kale

import kale.LOGIC.Variable
import org.scalatest.FreeSpec
import org.scalatest._

class KaleSpec extends FreeSpec {

  val X = Variable("X")

  "SET" - {
    val x = SET.SetOp
    println(x)
    println(INT.Int(2))
    println(SET.SetOp(INT.Int(2), INT.Int(3)))
    println(SET.SetOp())
  }

  "INT" - {
    import INT._
    import Implicits._

    val x: Token[Int] = implicitly[Token[Int]](2)
    val y: Token[Int] = implicitly[Token[Int]](3)

    assert(x == Int(2))
    assert(x != y)

    "Int" - {
      assert(x.value == 2)
      assert(x.label == INT.Int)
    }
    "+" - {
      assert(x + y == Int(5))
      assert(x + X != Int(5))
    }
  }
}