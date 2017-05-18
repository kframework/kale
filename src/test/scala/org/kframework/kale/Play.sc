
import language.dynamics

trait Foo extends Dynamic {
  def applyDynamic(m: String)(args: Any*) = {
    "dynamic"
  }
}

trait Bar extends Foo {
  def a(): String = {
    "a"
  }
}

val x: Foo = new Bar {}

x.a()

x.b()