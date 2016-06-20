package org.strategic
import scala.language.implicitConversions

trait Traversals {
  implicit class ProductTraversals(p : Product) {
    def walkTd(f : Any => _) {
      f(p)
      for(i <- p.productIterator)
        f(i)
    }
    
    def walkBu(f : Any => _) {
      for(i <- p.productIterator)
        f(i)
      f(p)
    }
  }
  
  implicit class TraversableTraversals(i : Traversable[Any]) {
    def walkTd(f : Any => _) {
      f(i)
      for(it <- i)
        f(it)
    }
    
    def walkBu(f : Any => _) {
      for(it <- i)
        f(it)
      f(i)
    }
  }
}