package org.kframework.kale

import org.kframework.kale.pretty.PrettyWrapperLabel

package object builtin {
  type hasINT = {
    val INT: INT
  }
  type hasBOOLEAN = {
    val BOOLEAN: BOOLEAN
  }
  type hasSTRING = {
    val STRING: STRING
  }
  type hasPrettyWrapper = {
    val PrettyWrapper: PrettyWrapperLabel
  }
}
