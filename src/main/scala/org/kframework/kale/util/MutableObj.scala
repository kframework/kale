package org.kframework.kale.util

import io.circe.{Decoder, Encoder, JsonObject, ObjectEncoder}

final class MutableObj[T](private var v: T) extends Mutable {
  def set(v: T): Unit = {
    this.v = v
  }

  def value(): T = v

  override def toString: String = "MutableObject(" + v + ")"

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: MutableObj[_] => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()

  override def clone(): MutableObj[T] = new MutableObj[T](v)
}

object MutableObj {
  implicit def encoder[T](implicit tEncoder: Encoder[T]): Encoder[MutableObj[T]] = tEncoder.contramap(_.v)
  implicit def decoder[T](implicit tDecoder: Decoder[T]): Decoder[MutableObj[T]] = tDecoder.map(new MutableObj[T](_))
}