package org.kframework.kale.util

import io.circe._
import io.circe.syntax._
import org.kframework.kale.Environment

case class AttCodec[T](att: Att[T], encoder: Encoder[T], decoder: Decoder[T])

class Codec(attCodecs: Set[AttCodec[E] forSome {type E}])(implicit val env: Environment) {

  import org.kframework.kale._

  implicit val termDecoder: Decoder[Term] = {
    val nameToAttDecoder: Map[String, AttCodec[E] forSome {type E}] = attCodecs map {
      case d@AttCodec(att, _, _) => (att.toString, d)
    } toMap

    Decoder.instance { (h: HCursor) =>
      val label = env.label(h.get[String]("label").right.get)

      val attsCursor: HCursor = h.downField("att").success.get

      val atts: Map[Att[_], _] = attsCursor.fieldSet.get map {
        (attName: String) =>
          val AttCodec(att, _, decoder) = nameToAttDecoder(attName)
          (att -> decoder(attsCursor.downField(attName).success.get).right.get)
      } toMap

      label match {
        case leafLabel: LeafLabel[_] =>
          val data = h.get[String]("data").right.get
          val res = leafLabel.interpret(data)
          res.attributes = atts
          Right(res)
        case nodeLabel: NodeLabel =>
          val decoder = Decoder.decodeList[Term]
          decoder(h.downField("children").success.get).map({ children =>
            val res = nodeLabel(children)
            res.attributes = atts
            res
          })
      }
    }
  }

  implicit val termEncoder: Encoder[Term] = {

    val nameToAttDecoder: Map[String, AttCodec[E] forSome {type E}] = attCodecs map {
      case d@AttCodec(att, _, _) => (att.toString, d)
    } toMap

    Encoder.instance[Term] { t =>
      val encodedAtts: Map[String, Json] = t.attributes map {
        case (att: Att[_], v: Any) => (att.toString -> nameToAttDecoder(att.toString).encoder.asInstanceOf[Encoder[Any]](v))
      }
      val labelAndAtts = Map("label" -> t.label.toString.asJson, "att" -> encodedAtts.asJson)
      t.label match {
        case label: LeafLabel[_] =>
          val label(data) = t
          (labelAndAtts + ("data" -> data.toString.asJson)).asJson
        case label: NodeLabel =>
          val node = t.asInstanceOf[Node]
          (labelAndAtts + ("children" -> node.children.asJson)).asJson
      }
    }
  }
}
