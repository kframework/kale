package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kore.Sort
import org.kframework.kale.transformer.Binary

import scala.collection._

trait importBuiltin
  extends Environment with builtin.BooleanMixin
    with builtin.IntMixin // with HasINTbop with HasINTcmp
    with builtin.IdMixin
    with builtin.StringMixin {

}

trait Z3Mixin extends importBuiltin {
  def SMTName(l: Label): String = l match {
    case INT.mod => "mod"
    case INT.lt => "<"
    case INT.gt => ">"
    case INT.le => "<="
    case INT.ge => ">="
  }

  def isZ3Builtin(l: Label): Boolean = l match {
    case _: Z3Builtin => true
    case l => INT.all.contains(l)
  }

  implicit class WithSMTname(l: Label) {
    def smtName: String = SMTName(l)
  }

}

trait MultisortedMixing extends Environment with standard.MatchingLogicMixin with standard.FreeMixin with Z3Mixin {

  private val sorts = mutable.Map[Label, Signature]()

  case class Signature(args: Seq[kale.Sort], target: kale.Sort)

  override def isSort(sort: Sort, term: Term): Boolean = sort == term.sort

  def sortArgs(l: Label): Seq[kale.Sort] = sorts.get(l).map({ signature => signature.args }).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  def sort(l: Label): kale.Sort = sorts.get(l).map({ signature => signature.target }).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  override def sort(l: Label, children: Seq[Term]): kale.Sort = sorts.get(l).map({ signature =>
    assert(children.map(_.sort) == signature.args)
    signature.target
  }).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  def sorted(l: Label, signature: Signature): Unit = {
    sorts.put(l, signature)
  }

  // TODO: move the util functions below somewhere else
  def sorted(l: LeafLabel[_], target: kale.Sort): Unit = sorted(l, Signature(Seq(), target))

  def sorted(l: Label0, target: kale.Sort): Unit = sorted(l, Signature(Seq(), target))

  def sorted(l: Label1, arg1: kale.Sort, target: kale.Sort): Unit = sorted(l, Signature(Seq(arg1), target))

  def sorted(l: Label2, arg1: kale.Sort, arg2: kale.Sort, target: kale.Sort): Unit = sorted(l, Signature(Seq(arg1, arg2), target))

  def sorted(l: Label3, arg1: kale.Sort, arg2: kale.Sort, arg3: kale.Sort, target: kale.Sort): Unit = sorted(l, Signature(Seq(arg1, arg2, arg3), target))

  override protected def makeMatcher = Binary.definePartialFunction({
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 || env.sort(l1) != env.sort(l2) => NoMatch
  }).orElse(super.makeMatcher)
}
