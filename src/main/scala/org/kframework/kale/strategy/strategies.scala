package org.kframework.kale.strategy

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.{ProcessingFunctions, definePartialFunction}
import org.kframework.kale.util.Named
import org.kframework.kale.{CluelessRoaring, ConjunctiveRoaring, DisjunctiveRoaring, Environment, FreeNode1, FreeNode2, FreeNode3, FunctionLabel1, FunctionLabel3, HasMatcher, Label1, Label2, Label3, Mixin, Node1, Predicate, Term, standard}
import org.kframework.km.term.Variable
import org.kframework.kore.Bottom
import org.roaringbitmap.RoaringBitmap

case class STRATEGY()(implicit env: Environment with standard.MatchingLogicMixin) {

  val anytimeIsNow = standard.lift("^nextIsNow", env.And.anytimeIsNow _)

  val onlyNonPredicate = standard.lift("^onlyNext", env.And.onlyNonPredicate _)

  trait Strategy {
    val isPredicate = Some(false)
  }

  val compose = new Named("^compose") with Label2 with Strategy with CluelessRoaring {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  val repeat = new Named("^repeat") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  def orElseLeave(t: Term): Term = orElse(t, env.Variable.freshVariable())

  val fixpoint = new Named("^fixpoint") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  /**
    * Takes a partial function
    */
  val bu = new Named("^bu") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  /**
    * Takes a partial function
    */
  val td = new Named("^td") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  val rw = new Named("^rewrite") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  val orElse = new Named("^orElse") with Label2 with Strategy with DisjunctiveRoaring {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  /**
    * ifThenElse(c, t, e) is semantically equivalent to Or(And(c, t), And(Not(c), t)) but evaluated lazily
    * i.e., the t and e are only touched when we know whether the condition is Top or Bottom
    */
  val ifThenElse = new Named("^ifThenElse") with FunctionLabel3 with Strategy {
    override def f(condition: Term, thenTerm: Term, elseTerm: Term): Option[Term] = condition match {
      case env.Top => Some(thenTerm)
      case env.Bottom => Some(elseTerm)
      case _ => None
    }
  }


  def noRewrite(t: Term) = !t.exists(_.label == env.Rewrite)

  /**
    * "Waits" for all non-anonymous variables to be instantiated, tries to match, and returns Top if unsat.
    */
  val doesNotMatch = new Named("!=") with Label2 with Predicate {
    override def apply(pattern: Term, obj: Term): Term =
      if (obj.variables.forall(v => v.name.str.startsWith("_"))) {
        val res = env.unify(pattern, obj)
        env.Truth(res == env.Bottom)
      } else {
        new FreeNode2(this, pattern, obj)
      }
  }

  /**
    * Matches/unifies it's argument and returns obj if unsat. See also doesNotMatch.
    */
  val unsat = new Named("unsat") with FunctionLabel1 with Strategy {
    override def f(_1: Term) = {
      val x = env.Variable("unsatVar" + env.Variable.counter)
      Some(env.And(x, doesNotMatch(_1, x)))
    }
  }
}

trait StrategyMixin extends Mixin {
  _: Environment with standard.MatchingLogicMixin with HasMatcher =>

  val STRATEGY = org.kframework.kale.strategy.STRATEGY()

  import STRATEGY._


  register(
    definePartialFunction({
      case (`orElse`, _) => orElseTerm
      case (`compose`, _) => composeTerm
      case (`repeat`, _) => repeatTerm
      case (`fixpoint`, _) => fixpointTerm
      case (`bu`, _) => buTerm
      case (`td`, _) => tdTerm
      case (`rw`, _) => rewriteTerm
    }), Priority.ultimate)

  // only works for ground obj
  case class orElseTerm(solver: Binary.Apply) extends Binary.F({ (orElse: Term, obj: Term) =>
    val STRATEGY.orElse(theThen, theElse) = orElse

    obj.asOr map { t =>
      val thenSol = unify(theThen, t)
      thenSol match {
        case Bottom => unify(theElse, t)
        case other => other
      }
    }
  })

  case class composeTerm(solver: Binary.Apply) extends Binary.F({ (composed: Term, obj: Term) =>
    val compose(f, g) = composed
    val matchG = unify(g, obj)
    val takeRelevantFromGMatch = anytimeIsNow(onlyNonPredicate(matchG))
    val matchF = unify(f, takeRelevantFromGMatch)

    matchF
  })

  case class repeatTerm(solver: Binary.Apply) extends Binary.F({ (fp: Term, obj: Term) =>
    val repeat(f) = fp
    val someVar = Variable.freshVariable()
    val sol = solver(orElse(f, Rewrite(someVar, someVar)), obj)
    sol.asOr map {
      case And.SPN(s, p, t) =>
        if (s.boundVariables.contains(someVar)) {
          And(p, Next(anytimeIsNow(t)))
        } else {
          solver(fp, anytimeIsNow(t)) // TODO: pass in the remaining predicates
        }
    }
  })

  case class fixpointTerm(solver: Binary.Apply) extends Binary.F({ (fp: Term, obj: Term) =>
    val fixpoint(f) = fp
    solver(f, obj) match {
      case Bottom => Bottom
      case Next(`obj`) => Next(obj)
      case res => solver(fp, And.anytimeIsNow(res))
    }
  })

  case class buTerm(solver: Binary.Apply) extends Binary.F({ (bu: Node1, obj: Term) =>
    val res = obj.mapBU(t => {
      val res = solver(bu._1, t)
      res match {
        case Bottom => t
        case _ => anytimeIsNow(onlyNonPredicate(res))
      }
    })
    res
  })

  case class tdTerm(solver: Binary.Apply) extends Binary.F({ (td: Node1, obj: Term) =>
    val res = obj.mapTD(t => {
      val res = solver(td._1, t)
      res match {
        case Bottom => t
        case _ => anytimeIsNow(onlyNonPredicate(res))
      }
    })
    res
  })

  case class rewriteTerm(solver: Binary.Apply) extends Binary.F({ (rewrite: Node1, obj: Term) =>
    rewrite._1.rewrite(obj) match {
      case Bottom =>
        Bottom
      case x =>
        Next(x)
    }
  })

}