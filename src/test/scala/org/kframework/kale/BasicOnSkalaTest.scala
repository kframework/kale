package org.kframework.kale

import org.kframework.backend.skala.SkalaBackend
import org.kframework.kore.extended.Backend
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Builders, parser}
import org.kframework.{kore => k}
import org.scalatest.FreeSpec
import org.kframework.kore.extended.implicits._


import scala.io.Source

object ProgrammaticBasicDefinition {

  import org.kframework.kore.implementation.DefaultBuilders._

  val sentences: Seq[k.Sentence] = Seq(
    SymbolDeclaration(Sort("Exp"), Symbol("_+_"), Seq(Sort("Int"), Sort("Int")), Attributes(Seq())),
    SymbolDeclaration(Sort("Exp"), Symbol("+Int"), Seq(Sort("Int"), Sort("Int")), Attributes(Seq())),
    Rule(Implies(Top(), And(Rewrite(Application(Symbol("_+_"), Seq(SortedVariable(Name("X"), Sort("Int")), SortedVariable(Name("Y"), Sort("Int")))),
      Application(Symbol("+Int"), Seq(SortedVariable(Name("X"), Sort("Int")), SortedVariable(Name("Y"), Sort("Int"))))), Next(Bottom()))), Attributes(Seq()))
  )

  val module: k.Module = Module(ModuleName("SEMANTICS"), sentences, Attributes(Seq()))

  val definition: k.Definition = Definition(Attributes(Seq()), Seq(module))

}

class BasicOnSkalaTest extends FreeSpec {


  //  "Basic" in {
  //    val defaultBuilders: Builders = DefaultBuilders
  //    val koreParser = parser.TextToKore(defaultBuilders)
  //    val imp = Source.fromResource("basic.kore")
  //    implicit val koreDefinition: k.Definition = koreParser.parse(imp)
  //
  //    //    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))
  //
  //    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))
  //
  //    val skalaBackend: Backend = SkalaBackend(koreDefinition, module)
  //  }

  "Basic" in {
    val defaultBuilders: Builders = DefaultBuilders
    implicit val koreDefinition: k.Definition = ProgrammaticBasicDefinition.definition

    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))

    val skalaBackend: Backend = SkalaBackend(koreDefinition, module)

    println(skalaBackend)
  }

}
