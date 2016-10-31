package util
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.runtime.universe._

// The following classes and their associated instances are used for implicit metaprogramming. 
// Do not EVER introduce implicits that have such types. Otherwise the Type Police will find you and reinterpret_cast you.

/* "Evidence" that `A` cannot be instantiated implicitly - unambigously or at all.
 * If there is no implicit value of type `A`, `![A]` can be instantiated implicitly.
 **/
sealed class ![A] private()
object !
{
	implicit def must_not_be_instantiatable[A] = new ![A]
	implicit def must_not_be_instantiatable_but_is[A](implicit a : A)
		= new ![A]
}

/* Evidence that at least one of `A` or ˙B` is implicitly instantiatable
 */
sealed class ||[A,B] private()
object ||
{
  implicit def both[A,B](implicit a : A, b : B) = new ||[A,B]
  implicit def a_only[A,B](implicit a : A, b_not : ![B]) = new ||[A,B]
  implicit def b_only[A,B](implicit a_not : ![A], b : B) = new ||[A,B]
}

/* Evidence that both `A` and `B` are implicitly instantiatable
 */
sealed class &&[A,B] private()
object &&
{
  implicit def both[A,B](implicit a : A, b : B) = new &&[A,B]  
}

/* Evidence that `U` is not equal to `V`
 */
sealed class =!=[U,V] private ()
object =!=
{
	implicit def must_not_be_equal[U, V] = new =!=[U,V] 
	
	implicit def must_not_be_equal_but_they_are[U,V](implicit counterevidence : U =:= V) = new =!=[U,V]
}

/* Evidence that `U` is not a subtype of `V`
 */
sealed class </< [U, V] private ()
object </<
{
	implicit def must_not_be_subtype[U, V] = new </<[U,V]
	
	implicit def must_not_be_subtype_but_they_are[U,V](implicit counterevidence : U <:< V) = new </<[U,V]
}

/* Evidence that `U` is not a supertype of `V`
 */
sealed class >/> [U, V] private ()
object >/>
{
	implicit def must_not_be_supertype[U, V] = new >/>[U,V]
	  
	implicit def must_not_be_supertype_but_they_are[U,V](implicit counterevidence : V <:< U) = new >/>[U,V]
}

/* Evidence that `U` is a strict supertype of `V` (i.e. a supertype of but not equal to)
 */
sealed class >>:>> [U, V] private ()
object >>:>>
{
	implicit def strict_supertype[U >: V,V](implicit strict : U =!= V) = new >>:>>[U,V]
}

/* Evidence that `U` is a strict subtype of `V` (i.e. a subtype of but not equal to)
 */
sealed class <<:<< [U, V] private ()
object <<:<<
{
	implicit def strict_subtype[U <: V,V](implicit strict : U =!= V) = new <<:<<[U,V]
}

/* Evidence the maximum of `A` and `B` exists and it is `M`.
 */
sealed class Max[A,B,M] private ()
object Max
{
	implicit def a_eq_b[A] = new Max[A,A,A]
	implicit def a_gt_b[A, B <: A](implicit neq : A =!= B) = new Max[A, B, A]
	implicit def a_lt_b[A, B >: A](implicit neq : A =!= B) = new Max[A, B, B]
}

/** Transitive closure of `R[_,_]`, i.e. `Tr[R, X, Y]` is true if `R[X,Y]` or `R[X,A] && Tr[R, A, Y]`
 */
sealed class Tr[R[_,_], X, Y] private()
object Tr {
  implicit def base[R[_,_], X, Y](implicit ev : R[X,Y]) = new Tr[R,X,Y]
  implicit def rec[R[_,_], X, A, Y](implicit ev : R[X,A] && Tr[R, A, Y]) = new Tr[R, X, Y]
}