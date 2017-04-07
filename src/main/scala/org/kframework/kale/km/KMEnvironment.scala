package org.kframework.kale.km

import org.kframework.kale.standard.{Bottomize, DNFEnvironment, HasINT, HasINTdiv}

class KMEnvironment extends DNFEnvironment with Bottomize with HasINT with HasINTdiv {
  implicit protected val env = this
}
