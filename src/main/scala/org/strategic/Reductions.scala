package org.strategic
import scala.language.implicitConversions

trait Reductions {
  implicit class ProductReductions(p : Product) {
    def collectAll[B](pf : PartialFunction[Any, B]) : List[B] = {
      def r(p : Product, acc : List[B]) : List[B] = {
        val nacc = 
              if(pf.isDefinedAt(p)) pf(p) :: acc
              else acc
        
        p.productIterator.foldLeft (nacc) { 
          (nacc, i) =>
            i match {
              case pp : Product => r(pp, nacc)
              case _ => nacc
            }
        }
      }
      
      r(p, Nil)
    }
  }
  
}