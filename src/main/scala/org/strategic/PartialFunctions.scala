package org.strategic

trait PartialFunctions {
  def ofType[A : Manifest] : PartialFunction[Any, A] = {
    case a : A => a
  }
  
  def where[A : Manifest](p : A => Boolean) : PartialFunction[Any, A] = {
    case a : A if p(a) => a
  }
}