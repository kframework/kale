package org.kframework.kale.km

import org.kframework.kale
import org.kframework.kale.builtin._
import org.kframework.kale.standard._
import org.kframework.kale._

import scala.collection._

trait importBuiltin
  extends Environment with importBOOLEAN
    with importINT // with HasINTbop with HasINTcmp
    with importID
    with importSTRING {

}

trait Z3Stuff extends importBuiltin {
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

class KMEnvironment extends standard.MatchingLogic with Z3Stuff {

  private val sorts = mutable.Map[Label, Signature]()

  case class Signature(args: Seq[kale.Sort], target: kale.Sort)

  def sortArgs(l: Label): Seq[kale.Sort] = sorts.get(l).map({ signature => signature.args }).getOrElse({
    throw new AssertionError("Could not find Signature for label: " + l)
  })

  def sortTarget(l: Label): kale.Sort = sorts.get(l).map({ signature => signature.target }).getOrElse({
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

  override lazy val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionApply(_)

  override protected lazy val unifier = new MultiSortedUnifier(this)

  override def rewrite(rule: Term, obj: Term): Term = ???
}
