package org.kframework.backend.skala
import org.kframework.kore.extended.implicits._
import org.kframework.kore
import org.kframework.kore.implementation.{DefaultBuilders => db}

/**
  * Implicits that add to Extended Kore
  */
object backendImplicits {
  implicit class ModuleWithSubsorting(m: kore.Module)(implicit definiton: kore.Definition) {
    private lazy val subsortRelations: Set[(kore.Sort, kore.Sort)] = m.allSentences.collect({
      case kore.SymbolDeclaration(startSort, _, Seq(endSort), att) if att.findSymbol(db.Symbol("klabel")).isEmpty => (endSort, startSort)
    }).toSet

    lazy val subsorts: POSet[kore.Sort] = POSet(subsortRelations)

    lazy val sortsFor: Map[kore.Symbol, Set[kore.Sort]] = m.allSentences.collect({
      case kore.SymbolDeclaration(s, sym, _, _) => (sym, s)
    }).groupBy(_._1).mapValues(_.map(_._2).toSet)
  }
}
