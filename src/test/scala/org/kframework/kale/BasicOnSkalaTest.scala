package org.kframework.kale

import org.kframework.backend.skala.SkalaBackend
import org.kframework.kale.standard.{SingleSortedMatcher, SubstitutionWithContext}
import org.kframework.kore.extended.Backend
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Builders, parser}
import org.kframework.{kore => k}
import org.scalatest.FreeSpec
import org.kframework.kore.extended.implicits._
import org.kframework.kore.parser.TextToKore

import scala.io.Source


//A programmatic Definition of Basic.
object ProgrammaticBasicDefinition {

  import org.kframework.kore.implementation.DefaultBuilders._

  /**
    * []
    * module SEMANTICS []
    * syntax Exp ::= `_+_`(Int, Int)    []
    * syntax Exp ::= `_+Int_`(Int, Int) [hook(INT.add)]
    *
    * rule X:Int + Y:Int => X +Int Y    []
    * endmodule
    */
  val sentences: Seq[k.Sentence] = Seq(
    SymbolDeclaration(Sort("Exp"), Symbol("_+_"), Seq(Sort("Int"), Sort("Int")), Attributes(Seq())),
    SymbolDeclaration(Sort("Exp"), Symbol("_+Int_"), Seq(Sort("Int"), Sort("Int")),
      Attributes(Seq(Application(Symbol("hook"), Seq(DomainValue(Symbol("AttributeValue"), Value("INT.add"))))))),
    Rule(Implies(Top(), And(Rewrite(Application(Symbol("_+_"), Seq(SortedVariable(Name("X"), Sort("Int")), SortedVariable(Name("Y"), Sort("Int")))),
      Application(Symbol("_+Int_"), Seq(SortedVariable(Name("X"), Sort("Int")), SortedVariable(Name("Y"), Sort("Int"))))), Next(Bottom()))), Attributes(Seq()))
  )

  val module: k.Module = Module(ModuleName("SEMANTICS"), sentences, Attributes(Seq()))

  val definition: k.Definition = Definition(Attributes(Seq()), Seq(module))

}


class BasicOnSkalaTest extends FreeSpec {

  val db: Builders = DefaultBuilders
  "In Basic," - {
//    "1 +Int 2 == 3" in {
//      implicit val koreDefinition: k.Definition = ProgrammaticBasicDefinition.definition
//
//      val module = koreDefinition.modulesMap(db.ModuleName("SEMANTICS"))
//
//      // Use Default Builders to Create the Definiton
//      val skalaBackend: Backend = SkalaBackend(koreDefinition, module)
//
//      // Use Builders of the Backend to Create Terms/Patterns
//      val pattern: k.Pattern = skalaBackend.Application(skalaBackend.Symbol("_+_"),
//        Seq(skalaBackend.DomainValue(skalaBackend.Symbol("Int"), skalaBackend.Value("1")),
//          skalaBackend.DomainValue(skalaBackend.Symbol("Int"), skalaBackend.Value("2"))))
//      assert(skalaBackend.step(pattern) == skalaBackend.DomainValue(skalaBackend.Symbol("Int"), skalaBackend.Value("3")))
//    }

    "Text Kore Parse and Rewrite" in {
      val parser: TextToKore = TextToKore(db)
      val basic = Source.fromResource("basic.kore")
      implicit val koreDefinition: k.Definition = parser.parse(basic)

      val module = koreDefinition.modulesMap(db.ModuleName("BOOL"))

      val skalaBackend: Backend = SkalaBackend(koreDefinition, module)

    }


  }

}
