import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Builders, Definition, parser}
import org.scalatest.FreeSpec

import scala.io.Source

class ImpTest() extends FreeSpec {
  val defaultBuilders: Builders = DefaultBuilders
  val koreParser = parser.TextToKore(defaultBuilders)
  val impDefinition = Source.fromFile("/Users/manasvi/Documents/k/k-distribution/tutorial/1_k/2_imp/lesson_5/imp-kompiled/kore.txt")
  val koreDefinition: Definition = koreParser.parse(impDefinition)

  implicit val env = new StandardEnvironment








}