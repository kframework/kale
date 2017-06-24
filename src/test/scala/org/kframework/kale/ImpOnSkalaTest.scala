package org.kframework.kale

import org.kframework.backend.skala.SkalaBackend
import org.kframework.kore.extended.Backend
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Builders, parser}
import org.kframework.{kore => k}
import org.scalatest.FreeSpec

import scala.io.Source

class ImpOnSkalaTest extends FreeSpec {

  "IMP" ignore {
    val defaultBuilders: Builders = DefaultBuilders
    val koreParser = parser.TextToKore(defaultBuilders)
    val imp = Source.fromResource("imp.kore")
    implicit val koreDefinition: k.Definition = koreParser.parse(imp)

    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))

//    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("INT-SYNTAX"))

    val skalaBackend: Backend = SkalaBackend(koreDefinition, module)
  }



}
