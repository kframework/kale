package org.kframework.kale.strategy

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.{ProcessingFunctions, definePartialFunction}
import org.kframework.kale.util.LabelNamed
import org.kframework.kale.{CluelessRoaring, ConjunctiveRoaring, DisjunctiveRoaring, Environment, SimpleNode1, SimpleNode2, SimpleNode3, FunctionLabel1, FunctionLabel3, HasMatcher, Label1, Label2, Label3, Mixin, MonoidLabel, Node1, Predicate, SemigroupLabel, Term, standard}
import org.roaringbitmap.RoaringBitmap

case class STRATEGY()(implicit env: Environment with standard.MatchingLogicMixin) {

  val anytimeIsNow = env.lift("^nextIsNow", env.And.anytimeIsNow _)

  val onlyNonPredicate = env.lift("^onlyNext", env.And.onlyNonPredicate _)

  trait Strategy {
    val isPredicate = Some(false)
  }

  val oneSolution = new LabelNamed("^oneSolution") with Label1 with Strategy with CluelessRoaring {
    override def apply(_1: Term): Term = SimpleNode1(this, _1)
  }

  val compose = new LabelNamed("^compose") with Label2 with Strategy with CluelessRoaring with SemigroupLabel {
    override def apply(_1: Term, _2: Term): Term = SimpleNode2(this, _1, _2)
  }

  val repeat = new LabelNamed("^repeat") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = SimpleNode1(this, f)
  }

  def orElseLeave(t: Term): Term = orElse(t, env.Variable.freshVariable())

  val fixpoint = new LabelNamed("^fixpoint") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = SimpleNode1(this, f)
  }

  /**
    * Takes a partial function
    */
  val bu = new LabelNamed("^bu") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = SimpleNode1(this, f)
  }

  /**
    * Takes a partial function
    */
  val td = new LabelNamed("^td") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = SimpleNode1(this, f)
  }

  /**
    * Takes a partial function. Similar to ^td, but returns Bottom if not applied anywhere.
    **/
  val topDown = new LabelNamed("^topDown") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = SimpleNode1(this, f)
  }

  val rw = new LabelNamed("^rewrite") with Label1 with Strategy with CluelessRoaring {
    override def apply(f: Term): Term = SimpleNode1(this, f)
  }

  val orElse = new LabelNamed("^orElse") with Label2 with Strategy with DisjunctiveRoaring with SemigroupLabel {
    override def apply(_1: Term, _2: Term): Term = SimpleNode2(this, _1, _2)
  }

  /**
    * ifThenElse(c, t, e) is semantically equivalent to Or(And(c, t), And(Not(c), t)) but evaluated lazily
    * i.e., the t and e are only touched when we know whether the condition is Top or Bottom
    */
  val ifThenElse = new LabelNamed("^ifThenElse") with FunctionLabel3 with Strategy {
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
  val doesNotMatch = new LabelNamed("!=") with Label2 with Predicate {
    override def apply(pattern: Term, obj: Term): Term =
      if ((pattern.variables | obj.variables).forall(v => v.name.toString.startsWith("_"))) {
        val res = env.unify(pattern, obj)
        env.Truth(res == env.Bottom)
      } else {
        new SimpleNode2(this, pattern, obj)
      }
  }

  /**
    * Matches/unifies it's argument and returns obj if unsat. See also doesNotMatch.
    */
  val unsat = new LabelNamed("unsat") with FunctionLabel1 with Strategy {
    override def f(_1: Term) = {
      val x = env.Variable("unsatVar" + env.Variable.counter)
      Some(env.Exists(x, env.And(x, doesNotMatch(_1, x))))
    }
  }
}

trait StrategyMixin extends Mixin {
  _: Environment with standard.MatchingLogicMixin with HasMatcher =>

  val STRATEGY = org.kframework.kale.strategy.STRATEGY()

  import STRATEGY._


  registerMatcher(
    {
      case (`orElse`, _) => orElseTerm
      case (`compose`, _) => composeTerm
      case (`repeat`, _) => repeatTerm
      case (`oneSolution`, _) => oneSolutionTerm
      case (`fixpoint`, _) => fixpointTerm
      case (`bu`, _) => buTerm
      case (`td`, _) => tdTerm
      case (`topDown`, _) => topDownTerm
    }, Priority.ultimate)

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

  case class oneSolutionTerm(solver: Binary.Apply) extends Binary.F({ (fp: Node1, obj: Term) =>
    val sol = solver(fp._1, obj)
    // TODO: make sure we pick deterministically
    env.Or.asSet(sol).headOption.getOrElse(env.Bottom)
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

  case class topDownTerm(solver: Binary.Apply) extends Binary.F({ (td: Node1, obj: Term) =>
    var matchedAtLeastOnce = false
    val res = obj.mapTD(t => {
      val res = solver(td._1, t)
      res match {
        case Bottom => t
        case _ =>
          matchedAtLeastOnce = true
          anytimeIsNow(onlyNonPredicate(res))
      }
    })
    if (matchedAtLeastOnce)
      res
    else
      Bottom
  })

}