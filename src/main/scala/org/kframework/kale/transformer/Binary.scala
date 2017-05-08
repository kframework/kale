package org.kframework.kale.transformer

import org.kframework.kale._

object Binary {

  trait TypedWith[L <: Term, R <: Term] {
    type Left = L
    type Right = R
  }

  object ProcessingFunction {
    implicit def functionToProcessingFunction[LeftIn <: Term, RightIn <: Term, SpecificSolver <: Apply](func: SpecificSolver => (LeftIn, RightIn) => Term): ProcessingFunction[SpecificSolver] = new ProcessingFunction[SpecificSolver] {
      override type Left = LeftIn
      override type Right = RightIn

      override def f(state: SpecificSolver)(l: Left, r: Right): Term = func(state)(l, r)
    }
  }

  /**
    * f specifies how to process a pair of terms with labels (leftLabel, rightLabel).
    * f is automatically hooked and applied via Apply.
    */
  trait ProcessingFunction[-SpecificSolver <: Apply] extends (SpecificSolver => ((Term, Term) => Term)) {
    type Left <: Term
    type Right <: Term

    def apply(solver: SpecificSolver): (Term, Term) => Term = {
      (a: Term, b: Term) => f(solver)(a.asInstanceOf[Left], b.asInstanceOf[Right])
    }

    def f(solver: SpecificSolver)(a: Left, b: Right): Term
  }

  trait Apply extends ((Term, Term) => Term) {
    val env: Environment
    assert(env.isSealed)

    type ProcessingFunctions = PartialFunction[(Label, Label), ProcessingFunction[this.type]]

    protected def definePartialFunction(f: ProcessingFunctions): ProcessingFunctions = f

    protected def processingFunctions: ProcessingFunctions = PartialFunction.empty

    protected lazy val arr: Array[Array[(Term, Term) => Term]] = {
      val pf = processingFunctions.lift

      val arr: Array[Array[(Term, Term) => Term]] =
        (0 until env.labels.size + 1).map({ i =>
          new Array[(Term, Term) => (Term)](env.labels.size + 1)
        }).toArray

      for (left <- env.labels) {
        for (right <- env.labels) {
          assert(arr(left.id)(right.id) == null)
          val f = pf((left, right)).map(_ (this)).orNull
          arr(left.id)(right.id) = f
        }
      }
      arr
    }

    def apply(left: Term, right: Term): Term = {
      //      assert(labels.contains(left.label) && labels.contains(right.label))
      assert(left.label.id <= env.labels.size, "Left label " + left.label + " with id " + left.label.id + " is not registered. Label list:" + env.labels.map(l => (l.id, l)).toList.sortBy(_._1).mkString("\n"))
      assert(right.label.id <= env.labels.size, "Right label " + right.label + " with id " + right.label.id + " is not registered. Label list:" + env.labels.map(l => (l.id, l)).toList.sortBy(_._1).mkString("\n"))

      val u = try {
        arr(left.label.id)(right.label.id)
      } catch {
        case _: IndexOutOfBoundsException => throw new AssertionError("No processing function registered for: " + left.label + " and " + right.label)
      }
      val res = if (u != null)
        u(left, right)
      else
        env.Bottom

      assert(!(left == right && res == env.Bottom), left.toString)
      res
    }
  }

}
