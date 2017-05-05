package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.builtin._
import org.kframework.kale.standard._
import org.kframework.kale.{Sort => _, _}

import scala.collection._

class KMEnvironment extends DNFEnvironment with HasINT with HasINTdiv with HasINTlt with HasINTle with HasINTgt with HasINTge with HasID {
  private implicit val env = this

  private val sorts = mutable.Map[Label, Signature]()

  case class Signature(args: Seq[Sort], target: Sort)

  override def sort(l: Label, children: Seq[Term]): kale.Sort = sorts.get(l).map({ signature =>
    assert(children.map(_.sort) == signature.args)
    signature.target
  }).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  def targetSort(l: Label): kale.Sort = sorts.get(l).map({signature => signature.target}).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  def sorted(l: Label, signature: Signature): Unit = {
    sorts.put(l, signature)
  }

  // TODO: move the util functions below somewhere else
  def sorted(l: LeafLabel[_], target: Sort): Unit = sorted(l, Signature(Seq(), target))

  def sorted(l: Label0, target: Sort): Unit = sorted(l, Signature(Seq(), target))

  def sorted(l: Label1, arg1: Sort, target: Sort): Unit = sorted(l, Signature(Seq(arg1), target))

  def sorted(l: Label2, arg1: Sort, arg2: Sort, target: Sort): Unit = sorted(l, Signature(Seq(arg1, arg2), target))

  def sorted(l: Label3, arg1: Sort, arg2: Sort, arg3: Sort, target: Sort): Unit = sorted(l, Signature(Seq(arg1, arg2, arg3), target))

  override val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionApply(_)
}
