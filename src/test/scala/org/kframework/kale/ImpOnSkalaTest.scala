package org.kframework.kale

import org.apache.commons.io.FileUtils
import org.kframework.backend.skala.SkalaBackend
import org.kframework.kore.Builders
import org.kframework.kore.implementation.DefaultBuilders
import org.scalatest.FreeSpec
import org.kframework.kore.parser
import org.kframework.{kore => k}
import org.kframework.kore.extended.Backend

import scala.io.Source
import org.kframework.kore.extended.implicits._

class ImpOnSkalaTest extends FreeSpec {

  "IMP" in {
    val defaultBuilders: Builders = DefaultBuilders
    val koreParser = parser.TextToKore(defaultBuilders)
    val imp = Source.fromResource("imp.kore")
    implicit val koreDefinition: k.Definition = koreParser.parse(imp)

    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))

//    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("INT-SYNTAX"))

    val skalaBackend: Backend = SkalaBackend(koreDefinition, module)
  }



}
