package org.kframework.kale.util

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import org.kframework.kale._
import org.kframework.kale.standard.{FreeLabel2, StandardEnvironment}
import org.scalatest.FreeSpec

import scala.language.implicitConversions

class CodecTest extends FreeSpec {

  implicit val env = new StandardEnvironment

  val foo = FreeLabel2("foo")

  import env._

  val pattern = foo(INT(3), STRING("bar"))

  object TestAtt extends Att[Int] {
    override def default(): Int = 0

    override def toString = "test"

    override def update(oldValue: Int, term: Term, oldChildren: Option[Iterable[Term]]): Int = oldValue
  }

  implicit def attEncoder(att: Att[_ <: Any]): Encoder[_] = att match {
    case TestAtt => Encoder.encodeInt.asInstanceOf[Encoder[Any]]
  }

  import org.kframework.kale.util.Codec._

  val attCodecs: Set[AttCodec[_]] = Set(
    AttCodec(TestAtt, Encoder.encodeInt, Decoder.decodeInt)
  )

  implicit val termDec: Decoder[Term] = termDecoder(env, attCodecs)
  implicit val termEnc: Encoder[Term] = termEncoder(attCodecs)

  "encode" in {
    val actual = pattern.asJson.noSpaces
    val expected = "{\"label\":\"foo\",\"att\":{},\"children\":[{\"label\":\"Int\",\"att\":{},\"data\":\"3\"},{\"label\":\"String\",\"att\":{},\"data\":\"bar\"}]}"
    assert(actual == expected)
  }

  "decode int" in {
    val actual = decode[Term]("{\"label\":\"Int\",\"att\":{},\"data\":\"3\"}")
    val expected = Right(INT(3))
    assert(actual === expected)
  }

  "decode string" in {
    val actual = decode[Term]("{\"label\":\"String\",\"att\":{},\"data\":\"bar\"}")
    val expected = Right(STRING("bar"))
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
    val expectedTerm: Term = INT(3)
    expectedTerm.att(TestAtt)

    val expectedJson = "{\"label\":\"Int\",\"att\":{\"test\":0},\"data\":\"3\"}"

    assertEncodings(expectedTerm, expectedJson)
  }
}