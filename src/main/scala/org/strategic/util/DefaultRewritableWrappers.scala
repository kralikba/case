package util
import org.strategic._

class SetWithIdsWrapper[K,V](val wrapped : SetWithIds[K,V]) extends RewritableWrapper[SetWithIds[K,V]]
{
  if(wrapped == null)
    throw new NullPointerException()
  
  override def all(s : Strategy) : Option[SetWithIdsWrapper[K,V]] = {
    var ok = true
    val n = wrapped.mapValues { _ match {
      case s(result) => result
      case x => ok=false; x
    }}
    if(ok) Some(new SetWithIdsWrapper(n))
    else None
  }
  
  override def one(s : Strategy) : Option[SetWithIdsWrapper[K,V]] = {
    var ok = false
    val n = wrapped.mapValues { (ok, _) match {
      case (false, s(result)) => ok=true; result
      case (_, x) => x
    }}
    if(ok) Some(new SetWithIdsWrapper(n))
    else None
  }
  
  override def canEqual(other : Any) = {
    other.isInstanceOf[SetWithIdsWrapper[_,_]] 
  }
  
  override def equals(other : Any) = {
    other match {
      case other : SetWithIdsWrapper[_, _] => wrapped.equals(other.wrapped)
      case _ => false
    }
  }
  
  override def productArity = wrapped.count
  
  override def productElement(n : Int) = wrapped.iterator.drop(n-1).next()
  
  override def productIterator = wrapped.iterator
}

object SetWithIdsRewritableWrapperProvider extends RewritableWrapperProvider[SetWithIds[_,_]] {  
  def get(o : SetWithIds[_,_]) : Option[SetWithIdsWrapper[_,_]] = {
    Some(new SetWithIdsWrapper(o))
  }  
}

object DefaultRewritableWrappers {
  RewritableWrapperProvider.register(SetWithIdsRewritableWrapperProvider)
  
  def load() = ()
}