package org.kframework.kale.standard

import org.kframework.kale.{Environment, Term}

trait BottomizeMixin {
  self: Environment =>

  var bottomizeIsActive = true

  def bottomize(_1: Term)(f: => Term): Term = {
    if (Bottom == _1 && bottomizeIsActive)
      Bottom
    else
      f
  }

  def bottomize(_1: Term, _2: Term)(f: => Term): Term = {
    if (Bottom == _1 || Bottom == _2 && bottomizeIsActive)
      Bottom
    else
      f
  }

  def bottomize(terms: Term*)(f: => Term): Term = {
    if (bottomizeIsActive)
      strongBottomize(terms: _*)(f)
    else
      f
  }

  def strongBottomize(terms: Term*)(f: => Term): Term = {
    if (terms.contains(Bottom))
      Bottom
    else
      f
  }
}
