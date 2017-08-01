package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.util.Named

trait MacroMixin {
  self: Environment with MatchingLogicMixin with TuplesMixing with StringMixin =>

  private val macros = collection.mutable.Map[String, (Term, Term)]()

  val macroDef = new Named("macro_def") with Label3 {
    override def apply(name: Term, signature: Term, body: Term): Term = name match {
      case STRING.String(name) =>
        if (macros.contains(name)) {
          throw new AssertionError("Macro " + name + " already defined.")
        }
        macros.put(name, (signature, body))
        Top
    }

    override val isPredicate: Option[Boolean] = None
  }

  val macroApply = new Named("macro_apply") with Label2 {
    override def apply(name: Term, args: Term): Term = name match {
      case STRING.String(key) =>
        val x = macros.get(key) map {
          case (signature, body) =>
            if (signature.children.size != args.children.size) {
              throw new AssertionError("Expected " + signature.children.size + " arguments for macro " + key + " but found " + args.children.size)
            }
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
        x getOrElse (throw new AssertionError("Macro " + key + " not found."))
    }

    override val isPredicate: Option[Boolean] = None
  }


}
