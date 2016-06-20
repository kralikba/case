package org.strategic
import util.ImmutableIdExtractor
import util.SetWithIds
import util.{!, &&}
import util.ForwardPipeable
import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros.{whitebox, blackbox}
import scala.reflect.api.Universe
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import util.IterableUtil
import scala.collection.{mutable,immutable}
//HINT: universe.showRaw{universe.reify { valami.copy("5", x = 7) }}

object Helper {
  /** Compares the two parameters for inequality:
   *  - if `A` is known to be `<:< AnyVal`, using `!=`
   *  - otherwise using [[AnyRef.ne]]
   */
  def rne[A](left : A, right : A) : Boolean = macro rneImpl[A]
  
  def rneImpl[A : _c.WeakTypeTag](_c : blackbox.Context)(
      left : _c.Expr[A], right : _c.Expr[A]) : _c.Expr[Boolean] = {
    import _c.universe._
    
    def hasParent[P : WeakTypeTag] = {
      weakTypeOf[A].baseClasses.contains(weakTypeOf[P].typeSymbol)
    }
    
    if(hasParent[AnyVal]) {
      _c.Expr[Boolean](q"""$left != $right""")
    } else if(hasParent[AnyRef]) {
      _c.Expr[Boolean](q"""$left ne $right""")
    } else {
      _c.Expr[Boolean](q"""$left.asInstanceOf[AnyRef] ne $right.asInstanceOf[AnyRef]""")
    } 
  }
  
  /** Compares the two parameters for equality:
   *  - if `A` is known to be `<:< AnyVal`, using `==`
   *  - otherwise using [[AnyRef.eq]]
   */
  def re[A](left : A, right : A) : Boolean = macro reImpl[A]
  
  def reImpl[A : _c.WeakTypeTag](_c : blackbox.Context)(
      left : _c.Expr[A], right : _c.Expr[A]) : _c.Expr[Boolean] = {
    import _c.universe._
    
    def hasParent[P : WeakTypeTag] = {
      weakTypeOf[A].baseClasses.contains(weakTypeOf[P].typeSymbol)
    }
    
    if(hasParent[AnyVal]) {
      _c.Expr[Boolean](q"""$left == $right""")
    } else if(hasParent[AnyRef]) {
      _c.Expr[Boolean](q"""$left eq $right""")
    } else {
      _c.Expr[Boolean](q"""$left.asInstanceOf[AnyRef] eq $right.asInstanceOf[AnyRef]""")
    } 
  }
}

/** Adds rewriting capabilities to an immutable non-case class as per trait [RewritableClass],
 *  makes main constructor parameters public (by-name parameters are turned into lazy vals),
 *  generates `hashCode` and `equals` functions based on these parameters, 
 *  and adds case class-like `apply` and `unapply` functions to the companion object.
 *  
 *  @remarks - The class's instances are assumed to be immutable and depend only on the parameters.
 *           - The class may only have one non-implicit parameter list.
 *           - No other parent type named `RewritableClass` may appear unqualified in the list of parent types. 
 *           - If [RewritableClass] explicitly appears in the list of parent types, it must do so unqualified
 *             and its type parameter must be the current type 
 *           - If an operation of [RewritableClass] (without [Product]) is overridden by the user, it will not be overwritten.
 *           - [Product.canEqual] may be overridden by the user; [Product.productArity] and [Product.productElement] not.
 *           - `apply` and `unapply` functions of the companion object will be generated regardless of 
 *             what the user has defined; conflicting signatures will cause compile errors 
 *           
 */
class rewritable extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any = macro rewritableImpl.impl 
}

object rewritableImpl {
  implicit class FlagSetOps[F <: Universe#FlagSet](left : F) {
    def &~(right : F) = {
      (left.asInstanceOf[Long] & ~ right.asInstanceOf[Long]).asInstanceOf[F]
    }
    
    def hasFlags(flags : F*) = {
      flags.forall(hasFlag(_))
    }
    
    def hasFlag(right : F) = {
      (left.asInstanceOf[Long] & right.asInstanceOf[Long]) == right.asInstanceOf[Long]
    }
  }
  
  def impl(_c : whitebox.Context)(annottees : _c.Expr[Any]*) : _c.Expr[Any] = {
    import _c.universe._
    import Flag._
    
    def error(msg : String) = 
      _c.abort(_c.enclosingPosition, msg)
      
    val l0 = annottees.map {_.tree}.toList
    
    val originalClassDef = 
      l0.collect { 
        case x : ClassDef => x 
      } match {
        case x :: Nil => x
        case Nil => error("@rewritable must be used on a class definition or its companion object")
        case _ => ???
      }
     
    val (c : ClassDef, params : List[ValDef]) = originalClassDef match {
    		case cd @ ClassDef(modifiers, name, tparams, Template(par, self, body)) =>
          val packagename = q"""_root_.org.strategic"""
          val suffix = "$byname"
          
          val tsym = tq"""$name[..${tparams map { _.name}}]"""

          /* Transform non-implicit, non-synthetic class parameters: 
           *  -for the by-value ones: make them public,
           *  -for the by-name ones: 
           *   -suffix their name with suffix [see above]
           *   -create a lazy val with the original name and an appropriate type (remove by-name from type)
           *  Transform the main constructor: 
           *   -suffix by-name parameters' names
           *   
           * returns a tuple, the first item of which is the transformed list of trees and
           *         the second item of which is a list of names and associated type expressions 
           *         of the class's transformed parameters
           */
          def transformBody(rem : List[Tree]) : (List[Tree], List[ValDef]) = {
            lazy val r = transformBody(rem.tail)
            rem match {
              case ValDef(_, TermName(s), _, _) :: _ if s.endsWith(suffix) =>
                error(s"@rewritable classes may not have user-defined vals the name of which end in '$suffix'")
              case (x @ ValDef(mods @ Modifiers(flags, p, a), vname @ TermName(s), t, b)) :: xs 
                if flags.hasFlag(PARAMACCESSOR) && !flags.hasFlag(SYNTHETIC) && !flags.hasFlag(IMPLICIT) =>
                if (flags.hasFlag(Flag.PROTECTED) || (flags.hasFlag(PRIVATE) && !flags.hasFlag(LOCAL)))
                  error("@rewritable classes may not have protected or private parameters")
                if (flags.hasFlag(Flag.BYNAMEPARAM)) { 
                  val nname = TermName(s ++ suffix)
                  val ntype = t match {
                    case tq"""$_[$nt]""" => nt
                    case _ => ???
                  }
                  val tflags = flags &~ PRIVATE &~ LOCAL &~ PARAMACCESSOR &~ BYNAMEPARAM | LAZY
                  val tv = ValDef(Modifiers(tflags, p, a), vname, ntype, q"""$name.this.$nname""")
                        
                  (ValDef(Modifiers(flags | PRIVATE | LOCAL, p, a), nname, t, b) :: tv :: r._1, tv :: r._2)
                } else {
                  val tv = ValDef(Modifiers(flags &~ PRIVATE &~ LOCAL, p, a), vname, t, b)
                  (tv :: r._1, tv :: r._2)
                }
              case DefDef(mods, cn @ termNames.CONSTRUCTOR, ctp @ List(), cvparams :: cvparamss, ct, 
                cbody @ q"""super.${termNames.CONSTRUCTOR}(); ()""") :: xs =>
                val nvparams = //suffix the by-name parameters' name
                  cvparams map { 
                    _ match {
                      case ValDef(vmods, TermName(vs), vtt, vb) if vmods.hasFlag(BYNAMEPARAM) =>
                        ValDef(vmods, TermName(vs ++ suffix), vtt, vb)
                      case v => v
                    }
                  }
                (DefDef(mods, cn, ctp, nvparams :: cvparamss, ct, cbody) :: r._1, r._2) 
              case x :: xs => (x :: r._1, r._2)
              case Nil => (Nil, Nil)
            }
          }
          
          
          /** Add RewritableClass[...] to list of parents, if necessary */
          def ensureRewritable(rem : List[Tree]) : List[Tree] = {
            rem match {
              case (x @ tq"""RewritableClass[..$rpar]""") :: xs if rpar.length == 1 =>
                rpar match {
                  case List(tq"""$tp""") if tp == tsym => x :: xs
                  case _ => error(s"expected: ${if (rem eq par) "extends" else "with"} RewritableClass[$name]")
                }
                case x :: xs => x :: ensureRewritable(xs)
                case Nil =>
                  tq"""$packagename.RewritableClass[$tsym]""" :: Nil
            }
          }
          
      		// Must not be a case class
      		if(modifiers.hasFlag(Flag.CASE))
      			error("@rewritable must be used on a non-case class")

    			val npar : List[Tree] = ensureRewritable(par)
    			val (tbody : List[Tree], params : List[ValDef]) = transformBody(body)
          
          def hasCombinator(name : String) : Boolean = {
            val tname = TermName(name)
            for(i <- tbody) {
              i match {
                case q"""$_ def ..$tname($_ : ${tq"""Strategy""" | tq"""_root_.org.strategic.Strategy"""}) : $_ = $_""" => 
                  return true
                case _ => ()
              }
            }
            return false
          }
          
          val defCopy : Tree = {
            val defpars = params.map { p => ValDef(Modifiers(PARAM), p.name, p.tpt, q"""this.${p.name}""") }
            
            q"""def copy(..$defpars) : $tsym = new $name(..${params map (_.name)})"""
          }
          
          val defAll : Tree = {
            if(hasCombinator("all")) q""
            else {
              val transformItems : List[Tree] = 
                (params map { p => val tn = p.name
                  List(q"""val $tn = f(this.$tn) match {
                         case Some(v) => v
                         case None => return None
                       }""", 
                       q"""changed = changed || $packagename.Helper.rne($tn, this.$tn)""")
                }).flatten
                
              q"""
                override def all(f : $packagename.Strategy) : Option[$tsym] = {
                  var changed = false
                  ..$transformItems
                  if(changed) Some(new $name(..${params.map(_.name)}))
                  else Some(this)
                }
              """
            } 
          }
          
          val defOne : Tree = {
            if(hasCombinator("one")) q""
            else {
              val items : List[Tree] = params map { p =>
                q"""f(this.${p.name}) match {
                    case Some(v) =>
                      if($packagename.Helper.rne(v, this.${p.name})) return Some(this.copy(${p.name} = v))
                      else return Some(this)
                    case None => ()
                  }"""
              }
              
              q"""override def one(f : $packagename.Strategy) : Option[$tsym] = {
                ..$items
                None
              }"""
            }
          }
          
          val defToString : Tree = {
            if(tbody.collect { case DefDef(_, TermName("toString"), List(), List(List()), tq"String", _) => true }.isEmpty) {
              val appendParams : List[Tree] = params match {
                case ValDef(_, n, _, _) :: xs => 
                  q"""sb.append(if(this.$n.asInstanceOf[AnyRef] == null) "<null>" else this.$n.toString)""" :: 
                  (xs map { p => q"""sb.append(",");sb.append(this.${p.name}.toString)"""})
                case Nil => Nil
              }
              q"""
                override def toString() = toStringAcyclic {
                  val sb = new scala.collection.mutable.StringBuilder(${name.toString + "("})
                  ..$appendParams
                  sb.append(")")
                  sb.toString()
                }
              """
            } else {
              q""
            }
          }
          
          val defCanEqual : Tree = {
            if(tbody.collect { 
              case DefDef(_, TermName("defCanEqual"), List(), List(ValDef(_, _, tq"Any", _)), tq"Boolean", _) => true }.isEmpty) {
              q"""
                override def canEqual(that : Any) : Boolean = that.isInstanceOf[$tsym]
                """ 
            } else {
              q""
            }
          }
          
          val defProductArity : Tree = q"""override def productArity = ${params.length}"""
          
          val defProductElement : Tree = {
            val cases = params zip (0 to params.length - 1) map { pp => cq"${pp._2} => this.${pp._1.name}" }
            q"""
              override def productElement(n : Int) : Any = {
                ${Match(q"n", cases)}
              }
            """
          }
          
          val defEquals : Tree = {
            val eqexpr = params
              .sortBy(!_.mods.hasFlag(LAZY)) // ensure that lazy parameters don't get evaluated unnecessarily 
              .foldLeft(q"true" : Tree) { (rem, p) =>
                q"this.${p.name} == other.${p.name} && $rem"
              }
              
            q"""
              override def equals(other : Any) : Boolean = {
                other match {
                  case other : $tsym => (this eq other) || eqAcyclic($eqexpr, other)
                  case _ => false
                }
              }  
            """
          }
          
          val defHashCode : List[Tree] = {
            val sumexpr = params.foldLeft(q"${scala.util.Random.nextInt}" : Tree) { (rem, p) =>
                q"if(this.${p.name}.asInstanceOf[AnyRef] == null) 0 else this.${p.name}.hashCode + $rem"
              } 
            
            List(q"""private lazy val hashCode$$$$ = hashCodeAcyclic($sumexpr)""",
                 q"""override def hashCode() : Int = hashCode$$$$""")
          }
          
          //TODO: implement productIterator, productPrefix
          
          val nbody = tbody ++ List(
              defCanEqual, 
              defProductArity, 
              defProductElement, 
              defCopy, 
              defAll, 
              defOne, 
              defToString,
              defEquals) ++ defHashCode
          
  			  (ClassDef(modifiers, name, tparams, Template(npar, self, nbody)), params)
  		}
    
    val m : ModuleDef = { l0.collect { case m1 : ModuleDef => m1 } 
      match {
        case m1 :: Nil => assert(l0.length == 2); m1
        case Nil => assert(l0.length == 1); q"""object ${TermName(c.name.toString)} { () }"""
        case _ => ???
      }} match {
        case m @ ModuleDef(modifiers, name, Template(par, self, body)) =>
          val tsym = tq"""${c.name}[..${c.tparams map { _.name }}]"""
          
          val defApply : Tree = {
            val pars = params.map { p => ValDef(Modifiers(PARAM), p.name, p.tpt, q"") }
            val implicits = c.impl.body collect {
              case ValDef(m, name, tpe, _) if m.flags.hasFlags(PARAMACCESSOR, IMPLICIT) => 
                ValDef(Modifiers(PARAM | IMPLICIT), name, tpe, q"")
            }
            val defpars : List[List[ValDef]] = List(pars, implicits)
            q"""def apply[..${c.tparams}]() : $tsym = new ${c.name}(..${params.map(_.name)})""" match {
              case DefDef(m, n, tp, _, tpt, rhs) => DefDef(m, n, tp, defpars, tpt, rhs)
              case _ => ???
            } 
          }
          
          val defUnapply : Tree = {
            q"""
              def unapply[..${c.tparams}](instance : $tsym) : Option[(..${params.map(_.tpt)})] =
                Some((..${params.map { p => q"""instance.${p.name}""" }}))
              """
          }
          
          val nbody = body ++ List(defApply, defUnapply)
          ModuleDef(modifiers, name, Template(par, self, nbody))
      }
    
//    println("c:")
//    println(c)
//    println(showRaw(c))
//    
//    println("m:")
//    println(m)
//    println(showRaw(m))
//    
    _c.Expr[Any](q"""..$c ; ..$m""")
  }
}