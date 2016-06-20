package org.strategic
import scala.collection.{mutable, immutable}
import org.strategic.RewritableWrapper
import org.strategic.RewritableWrapperProvider

trait RewritableClass[+This] extends Product {
  /** Applies a type-preserving transformation to each of its items, then builds a new `This`
   *  using the transformed items. 
   */
  def all(s : Strategy) : Option[This]
  
  /** Tries to apply a type-preserving transformation to at least one of its items, then builds a new `This`
   *  using the transformed item and preserving the others. 
   */
  def one(s : Strategy) : Option[This]
  
  private lazy val superHash = super.hashCode()
  
  /** Protective wrapper for possibly hash code calculation against infinite call chains.
   *  If, in the current thread's call stack, [hashAcyclic] has already been called for the
   *  object `o`, returns `0`; if not, evaluates and returns `ifNew`.
   *  @notes should not be directly called.
   *          `def hashCode()` generated through `@rewritable` will call it and assume 
   *          that nothing else did.
   */
  
  /** Protective wrapper for possibly recursive equality checking against infinite call chains. 
   *  If, in the current thread's call stack, [eqAcyclic] has already been called for the object
   *  `l` and `r`, returns `true`; otherwise evaluates and returns `ifNew`.
   *  
   *  @remarks should not be directly called.
   *          `def equals(other : Any)` generated through `@rewritable` will call it and assume 
   *          that nothing else did.
   */
  protected def eqAcyclic(ifNew : => Boolean, other : RewritableClass[_]) : Boolean = {
    import RewritableClass._
    val f = new eqStackFrame(this, other)
    val st = eqStack.get
    if(st.contains(f)) true
    else {
      eqStack.set(st + f)
      try {
        ifNew
      }
      finally {
        eqStack.set(st)
      }
    }
  }
  
  protected def hashCodeAcyclic(ifNew : => Int) : Int = {
    thisAcyclic(ifNew, 0, RewritableClass.hashStack)
  }
  
  protected def toStringAcyclic(ifNew : => String) : String = {
    thisAcyclic(ifNew, "<...>", RewritableClass.toStringStack)
  }
   
  protected def thisAcyclic[A](ifNew : => A, default : A,
      tl : ThreadLocal[immutable.HashSet[RewritableClass.thisStackFrame]]) : A = {
    import RewritableClass._
    val f = new thisStackFrame(this)
    val st = tl.get
    if(st.contains(f)) default
    else {
      tl.set(st + f)
      try {
        ifNew
      }
      finally {
        tl.set(st)
      }
    }
  } 
}
object RewritableClass {
  private final class eqStackFrame(val l : RewritableClass[_], val r : RewritableClass[_]) {
    override def hashCode() = l.superHash + r.superHash
    override def equals(other : Any) = {
      other match {
        case other : eqStackFrame =>
          (l eq other.l) && (r eq other.r)
        case _ => false
      }
    }
  }
  
  private final val eqStack = new ThreadLocal[immutable.HashSet[eqStackFrame]] {
    override def initialValue() = immutable.HashSet.empty
  }
  
  private final class thisStackFrame(val o : RewritableClass[_]) {
    override def hashCode() = o.superHash
    override def equals(other : Any) = {
      other match {
        case other : thisStackFrame =>
          o eq other.o
        case _ => false
      }
    }
  }
  
  
  private final val hashStack = new ThreadLocal[immutable.HashSet[thisStackFrame]] {
    override def initialValue() = immutable.HashSet.empty
  }
  
  private final val toStringStack = new ThreadLocal[immutable.HashSet[thisStackFrame]] {
    override def initialValue() = immutable.HashSet.empty
  }
}