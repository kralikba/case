package org.strategic
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.api.Universe

/** A Strategy is an attempt at a type-preserving transformation.
 */
abstract class Strategy {
  def apply[A](a : => A) : Option[A]
  def unapply[A](a : A) : Option[A] = apply(a)
}
object Strategy {
  def apply[A : Manifest](pf : PartialFunction[A,A]) = new Strategy {
    override def apply[B](a : => B) : Option[B] = {
      a match {
        case a : A => pf.lift(a).asInstanceOf[Option[B]]
        case _ => None
      }
    }
  }
  
  def apply[A : Manifest](f : A => A) = new Strategy {
    override def apply[B](a : => B) : Option[B] = {
      a match {
        case a : A => Some(f(a).asInstanceOf[B])
        case _ => None
      }
    }
  }
}