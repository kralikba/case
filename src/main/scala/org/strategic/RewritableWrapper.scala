package org.strategic
import scala.collection.mutable.MutableList

trait RewritableWrapper[+A] extends RewritableClass[RewritableWrapper[A]] {
  val wrapped : A
}
object RewritableWrapper
{
  def unapply[A](o : A) : Option[RewritableWrapper[A]] = {
    RewritableWrapperProvider.get(o)
  }  
}

trait RewritableWrapperProvider[A] {
  def get(o : A) : Option[RewritableWrapper[A]] 
}

object RewritableWrapperProvider {
  private class ProviderRecord[A](val prov : RewritableWrapperProvider[A])(implicit val m : Manifest[A]) 
  {
    def unapply(a : Any) : Option[RewritableWrapper[A]] = {
      a match {
        case a : A => prov.get(a)
        case _ => None
      }
    }
  }
  
  private val providers = new MutableList[ProviderRecord[_]] 
  
  def register[A : Manifest](p : RewritableWrapperProvider[A]) { 
    if(!providers.exists { _.prov eq p })
      providers += new ProviderRecord(p)
  }
  
  def get[A](o : A) : Option[RewritableWrapper[A]] = {
    for(p <- providers) {
      o match {
        case p(w : RewritableWrapper[A]) => return Some(w)
        case _ => ()
      }
    }
    return None
  }
  
  util.DefaultRewritableWrappers.load
}