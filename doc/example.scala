import com.fonmoney.fancycase.FancyTraitCompanion
import shapeless.HNil

/**
  * The abstract vals introduced in a `@fancy trait` are called its fields (i.e. without any inherited ones.)
  * vals with a right-hand-side may be marked as `abstract`; that will then be removed, treated as
  * an optional field and the right-hand-side will be its default value.
  * These must always be the last ones.
  *
  * The fields of `@fancy traits` are expected to be `replacable` in their implementors,
  * i.e. there should be a virtual method that copies the current object but replaces
  *      the values of the abstract fields of the trait.
  **/

@fancy trait A {
  val i : Int
  val s : String
  abstract val e : Boolean = false
}

/** will be equivalent to:
*/

trait A {
  val i : Int
  val s : String
  val e : Boolean = false

  /** The type of the implementing type. Should be overridden in all subtypes. */
  type Self >: this.type <: A

  /** A copy of the current object such that everything is unchanged but values i and s */
  def withA(i: Int = i, s: String = s, e : Boolean = e): Self

  def withA(newValues: A.Repr): Self = newValues match {
    case i :: s :: e :: _ => withA(i, s)
  }

  /** `val r = (x : A).withoutA` can be passed around as a "remainder" of dividing x by A. Then,
    * `r * (i, s)` will be equivalent to `x.withA(i,s)`
    **/
  def withoutA: A.Remainder[Self] = new A.Remainder(this)

  /** Determines whether two objects' fields (in A) are equal.
    */
  final def equalsInA(other : A) = i == other.i && s == other.s && e == other.e
}
object A extends FancyTraitCompanion[A] {
  type Repr = Int :: String :: HNil
  object Repr {
    def apply(i : Int, s : String, e : Boolean = false) : Repr = i :: s :: HNil
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
  abstract val d : Double = 3.14
}

/** Then, a `@fancy case class` will automatically implement these traits.
  * Their mandatory fields will be inserted into the primary constructor's first parameter list, in order of linearization,
  * between the mandatory and the optional parameters; the optional ones after them.
  * A `fromComponents` method will also be added to the companion object, which is similar to the default apply method,
  * but instead of the ancestor's fields, accepts instances of the ancestor fancy traits.
  **/

@fancy case class C(f : Boolean, x : Boolean = false) extends Q with A with B

/** is equivalent to */

case class C(f : Boolean,
             i : Int, s : String,
             l : Long,
             j : Int, k : Int,
             x : Boolean = false,
             e : Boolean = A.Repr.apply$defaults$3,
             d : Double = Q.Repr.apply$defaults$2) extends Q with A with B {
  type Self = C

  override def withA(i : Int = i, s : String = s, e : Boolean = e) = copy(i = i, s = s)
  override def withQ(l : Long = l, d : Double = d) = copy(l = l, d = d)
  override def withB(j : Int = j, k : Int = k) = copy(j = j, k = k)
}
object C {
  def fromComponents(f : Boolean, A : A, Q : Q, B : B, x : Boolean = false) = {
    C(f, A.i, A.s, Q.l, B.j, B.k, x)
  }
}