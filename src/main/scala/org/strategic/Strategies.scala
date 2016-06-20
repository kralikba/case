package org.strategic
import scala.language.implicitConversions

trait Strategies {
  val id = new Strategy {
    override def apply[A](a : => A) : Option[A] = Some(a)
  }
  
  def all(s : Strategy) = new Strategy {
    override def apply[A](a : => A) : Option[A] = {
      a match {
        case r : RewritableClass[A] => 
          r.all(s)
        case RewritableWrapper(w) =>
          w.all(s) map { _.wrapped }
        case o => Some(o) 
      }
    }
  } 
  
  def one(s : => Strategy) = new Strategy {
    override def apply[A](a : => A) : Option[A] = {
      a match {
        case r : RewritableClass[A] => 
          r.one(s)
        case RewritableWrapper(w) =>
          w.one(s) map { _.wrapped }
        case _ => None
      }
    }
  }
  
  def trys(s : => Strategy) = new Strategy {
    override def apply[A](a : => A) : Option[A] = {
      s(a) match {
        case Some(r) => Some(r)
        case None => Some(a)
      }
    }
  }
  
  implicit class StrategyOps(left : => Strategy) {
    def *(right : => Strategy) = new Strategy {
      override def apply[A](a : => A) : Option[A] = {
        left(a) match {
          case Some(i) => right(i)
          case None => None
        }
      }
    }
    
    def <+(right : => Strategy) = new Strategy {
      override def apply[A](a : => A) : Option[A] = {
        left(a) match {
          case r @ Some(_) => r
          case None => right(a)
        }
      }
    }
    
    def +(right : => Strategy) = new Strategy {
      override def apply[A](a : => A) : Option[A] = {
        left(a) match {
          case r @ Some(_) => r
          case None => right(a)
        }
      }
    }
    
    def ?(t : => Strategy, f : => Strategy) = new Strategy {
      override def apply[A](a : => A) : Option[A] = {
        left(a) match {
          case Some(_) => t(a)
          case None => f(a)
        }
      }
    }
  }
  
  def alltd(s : => Strategy) : Strategy = s * all(alltd(s))
  def allbu(s : => Strategy) : Strategy = all(allbu(s)) * s
}