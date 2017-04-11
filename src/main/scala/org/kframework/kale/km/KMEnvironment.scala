package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.{Sort => _, _}
import org.kframework.kale.standard._

import collection._

class KMEnvironment extends DNFEnvironment with HasINT with HasINTdiv with HasID {
  implicit protected val env = this

  private var sorts = Map[Label, Signature]()

  case class Signature(args: Seq[Sort], target: Sort)

  override def sort(l: Label, children: Seq[Term]): kale.Sort = sorts.get(l).map({ signature =>
    assert(children.map(_.sort) == signature.args)
    signature.target
  }).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  def sorted(l: Label, signature: Signature): Unit = {
    sorts += (l -> signature)
  }

  // TODO: move the util functions below somewhere else
  def sorted(l: LeafLabel[_], sort: Sort): Unit = sorted(l, Signature(Seq(), sort))

  def sorted(l: Label0, sort: Sort): Unit = sorted(l, Signature(Seq(), sort))

  def sorted(l: Label1, arg: Sort, target: Sort): Unit = sorted(l, Signature(Seq(arg), target))

  def sorted(l: Label2, arg1: Sort, arg2: Sort, target: Sort): Unit = sorted(l, Signature(Seq(arg1, arg2), target))

  def sorted(l: Label3, arg1: Sort, arg2: Sort, arg3: Sort, target: Sort): Unit = sorted(l, Signature(Seq(arg1, arg2, arg3), target))
}
