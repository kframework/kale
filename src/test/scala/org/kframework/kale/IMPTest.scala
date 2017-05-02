import org.apache.commons.io.FileUtils
import org.kframework.kale.Environment
import org.kframework.kale.kore.{KaleBackend, KaleRewriter}
import org.kframework.kale.standard.{KaleBuilders, StandardEnvironment}
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Builders, _}
import org.scalatest.FreeSpec

import scala.io.Source

// kale

class IMPTest() extends FreeSpec {

  "IMP" in {
    val defaultBuilders: Builders = DefaultBuilders
    val koreParser = parser.TextToKore(defaultBuilders)
    val impKoreFile = FileUtils.toFile(getClass.getResource("imp.kore"))
    val koreDefinition: Definition = koreParser.parse(impKoreFile)

    val makeBackend: MakeBackend = KaleBackend

    val kaleBackend: Backend = makeBackend(koreDefinition, defaultBuilders.ModuleName("IMP"))

    import kaleBackend._
  }
}
