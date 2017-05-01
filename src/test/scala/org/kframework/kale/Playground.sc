import org.kframework.kale._
import org.kframework.kale.util.Implicits
import standard._

implicit val env = new StandardEnvironment

import env._

val impl = new Implicits()

import impl._

val block = SimpleFreeLabel1("block")

val mult = SimpleFreeLabel2("mult")

env.seal()

val m = SingleSortedMatcher()

val s = new SubstitutionApply(_)

// ....

val X = Variable("X")
val Y = Variable("Y")

//

val r = Rewriter(s, m, env)(
  Set(env.Rewrite(mult(X, 0), 0: Term))
)

val m1 = m(
  mult(X, 0),
  mult(3, 0))

m1 match {
  case sub: Substitution =>
}





