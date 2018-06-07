package org.kframework.kale.util

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.kframework.kale._
import org.kframework.kale.standard.{StandardEnvironment}
import org.scalatest.FreeSpec

import scala.language.implicitConversions

class CodecTest extends FreeSpec {

  implicit val env = StandardEnvironment()

  import env._

  val foo = FreeLabel2("foo")

  import env._

  val pattern = foo(3, STRING.String("bar"))

  object TestAtt extends Att[Int] {
    override def toString = "test"

    override def update(term: Term, oldTerm: Option[Term]): Int = 0
  }

  val codec = new Codec(Set(
    AttCodec(TestAtt, Encoder.encodeInt, Decoder.decodeInt)
  ))

  import codec._

  "encode" in {
    val actual = pattern.asJson.noSpaces
    val expected = "{\"label\":\"foo\",\"children\":[{\"label\":\"Int@INT-SYNTAX\",\"data\":\"3\"},{\"label\":\"String\",\"data\":\"bar\"}]}"
    assert(actual == expected)
  }

  "decode int" in {
    val actual = decode[Term]("{\"label\":\"Int@INT-SYNTAX\",\"att\":{},\"data\":\"3\"}")
    val expected = Right(INT.Int(3))
    assert(actual === expected)
  }

  "decode string" in {
    val actual = decode[Term]("{\"label\":\"String\",\"att\":{},\"data\":\"bar\"}")
    val expected = Right(STRING.String("bar"))
    assert(actual === expected)
  }

  "round trip" in {
    val json = pattern.asJson.noSpaces
    val afterRoundTrip = decode[Term](json).right.get

    assert(pattern === afterRoundTrip)
  }

  private def assertEncodings(expectedTerm: Term, expectedJson: String) = {
    assert(expectedTerm.asJson.noSpaces === expectedJson)

    assert(decode[Term](expectedJson) === Right(expectedTerm))

    assert(decode[Term](expectedJson).right.get.attributes === expectedTerm.attributes)
  }

  "att encoding" in {
    val expectedTerm: Term = INT.Int(3)
    expectedTerm.att(TestAtt)

    val expectedJson = "{\"label\":\"Int@INT-SYNTAX\",\"att\":{\"test\":0},\"data\":\"3\"}"

    assertEncodings(expectedTerm, expectedJson)
  }

  "mutable obj encoding" in {
    val x = new MutableObj(4)
    assert(x.asJson.noSpaces === "4")
    assert(decode[MutableObj[Int]]("4") === Right(new MutableObj(4)))
  }
}