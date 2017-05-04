package org.kframework.kale

import org.kframework.backend.skala.SkalaBackend
import org.kframework.kore.extended.Backend
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Builders, parser}
import org.kframework.{kore => k}
import org.scalatest.FreeSpec
import org.kframework.kore.extended.implicits._


import scala.io.Source


class BasicOnSkalaTest extends FreeSpec{


  "Basic" in {
    val defaultBuilders: Builders = DefaultBuilders
    val koreParser = parser.TextToKore(defaultBuilders)
    val imp = Source.fromResource("basic.kore")
    implicit val koreDefinition: k.Definition = koreParser.parse(imp)

    //    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))

    val module = koreDefinition.modulesMap(defaultBuilders.ModuleName("SEMANTICS"))

    val skalaBackend: Backend = SkalaBackend(koreDefinition, module)
  }


}
