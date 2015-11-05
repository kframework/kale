package kale

import scala.language.implicitConversions

object Implicits {

  implicit def intToken(x: Int): Token[Int] = INT.Int(x)
  implicit class IntWithOps(x: Token[Int]) {
    def +(y: Term) = INT.+(x, y)
  }
}
