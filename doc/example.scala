import com.fonmoney.fancycase.FancyTraitCompanion
import shapeless.HNil

/**
  * The abstract vals introduced in a `@fancy trait` are called its fields (i.e. without any inherited ones.)
  * The fields of `@fancy traits` are expected to be `replacable` in their implementors,
  * i.e. there should be a virtual method that copies the current object but replaces
  *      the values of the abstract fields of the trait.
  **/

@fancy trait A {
  val i : Int
  val s : String
}

/** will be equivalent to:
*/

trait A {
  val i : Int
  val s : String

  /** The type of the implementing type. Should be overridden in all subtypes. */
  type Self >: this.type <: A

  /** A copy of the current object such that everything is unchanged but values i and s */
  def withA(i: Int, s: String): Self

  def withA(newValues: A.Repr): Self = newValues match {
    case i :: s :: _ => withA(i, s)
  }

  /** `val r = (x : A).withoutA` can be passed around as a "remainder" of dividing x by A. Then,
    * `r * (i, s)` will be equivalent to `x.withA(i,s)`
    **/
  def withoutA: A.Remainder[Self] = new A.Remainder(this)
}
object A extends FancyTraitCompanion[A] {
  type Repr = Int :: String :: HNil
  object Repr {
    def apply(i : Int, s : String) : Repr = i :: s :: HNil
  }

  class Remainder[+Dividend <: A](of : Dividend { type Self <: Dividend }) {
    def *(i : Int = of.i, s : String = of.s) : Dividend = of.withA(i,s)
    def *(right : Repr) : Dividend = of.withA(right)
  }

  /** Implementors of A can be pattern matched upon without knowing its runtime type.
    * {{{
    * (x : A) match {
    *   case A((i,s), rem) => rem * (i,s)
    * }
    * }}}
    * will then equal x.
    */
  def unapply[T <: A](instance : T { type Self <: T }) : Option[((Int, String), Remainder[T])] = {
    Some((instance.i, instace.s), instance.withoutA)
  }
}

/** Suppose we have the following definitions as well:
*/
@fancy trait B {
  val j : Int
  val k : Int
}

@fancy trait Q extends A {
  val l : Long
}

/** Then, a `@fancy case class` will automatically implement these traits.
  * Their fields will be appended to the primary constructor's first parameter list, in order of linearization.
  **/

@fancy case class C(f : Boolean) extends Q with A with B

/** is equivalent to */

case class C(f : Boolean, i : Int, s : String, l : Long, j : Int, k : Int) extends  Q with A with B {
  type Self = C

  override def withA(i : Int, s : String) = copy(i = i, s = s)
  override def withQ(l : Long) = copy(l = l)
  override def withB(j : Int, k : Int) = copy(j = j, k = k)
}