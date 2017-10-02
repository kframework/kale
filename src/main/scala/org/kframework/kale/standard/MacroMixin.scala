package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.util.Named

trait MacroMixin {
  _: Environment with MatchingLogicMixin with TuplesMixin with StringMixin =>

  private val macros = collection.mutable.Map[String, (Term, Term)]()

  case class MacroException(msg: String) extends Exception(msg)

  val macroDef = new Named("macro_def") with Label3 {
    override def apply(name: Term, signature: Term, body: Term): Term = name match {
      case STRING.String(name) =>
        defineMacro(name, signature, body)
        Top
    }

    override val isPredicate: Option[Boolean] = None
  }

  def defineMacro(name: String, signature: Term, body: Term, allowRedeclare: Boolean = false) {
    if (!allowRedeclare && macros.contains(name)) {
      throw MacroException("Macro " + name + " already defined.")
    }
    macros.put(name, (signature, body))
  }

  def macroIsDefined(name: String): Boolean = macros.contains(name)

  val macroApply = new Named("macro_apply") with Label2 {
    override def apply(name: Term, args: Term): Term = name match {
      case STRING.String(key) =>
        val x = macros.get(key) map {
          case (signature, body) =>
            if (signature.children.size != args.children.size) {
              throw MacroException("Expected " + signature.children.size + " arguments for macro " + key + " but found " + args.children.size)
            }
            if (signature.children.isEmpty) {
              body
            } else {
              val transform = signature.children
                .zip(args.children)
                .toMap
                .withDefault({
                  case v: Variable if v.name.str.startsWith("_") =>
                    Variable.freshVariable()
                  case t => t
                })

              val res = body.mapBU(transform)
              res
            }
        }
        x getOrElse (throw MacroException("Macro " + key + " not found."))
    }

    override val isPredicate: Option[Boolean] = None
  }


}
