package com.fonmoney.fancycase
import shapeless.HNil
import scala.annotation._
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
/**
  * Created by kralikba on 2016.10.31..
  */
/** See `doc/example.scala`
  **/
@compileTimeOnly("!!! Enable macro paradise for processing annotations")
class fancy extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any = macro fancy.impl
}
object fancy {
  final val replacePrefix = "with"
  final val decomposePrefix = "without"

  def impl(c : Context)(annottees : c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val util = TreesUtil[c.type](c)
    import util._

    val companionTrait = typeOf[FancyTraitCompanion[_]]

    annottees.map(_.tree).toList match {
      case q"${mods: Modifiers} class $name[..$tp] $cmods(...$cparams) extends { ..$early } with ..$parents { $self => ..$body }" :: xs
        if (mods.hasFlag(Flag.CASE)) =>
        // case classes will get default implementations of @fancy traits
        val fancyTraits = {
          val ptpe = parents.map { p => resolveType(p) }.toList
          def linearize(types : List[Type], acc : List[TypeSymbol]) : List[Type] = {
            types match {
              case t :: ts if acc.contains(t.typeSymbol) => linearize(ts, acc)
              case t :: ts =>
                val nextAncestors = t.baseClasses.map{_.asType}.filterNot{c => acc.contains(c)}.reverse
                linearize(ts, (acc ++ nextAncestors))
              case _ => acc map { sym => resolveType(tq"$sym") }
            }
          }
          linearize(ptpe, List()) filter { t =>
            if(t.companion =:= NoType) false
            else t.companion <:< companionTrait
          }
        }
        val fieldsByTrait = fancyTraits map { t =>
          val fields = t
            .companion
            .member(TermName("Repr")).typeSignature
            .member(TermName("apply")).typeSignature
            .paramLists.head map { _.asTerm }
          (t, fields)
        }
        val cparams1 : Seq[Seq[Tree]] = {
          val add : Seq[Tree] = {
            fieldsByTrait
            .map { _._2 }
            .flatten
            .map { sym => q"val ${sym.name.toTermName} : ${sym.typeSignature}"}
          }
          if(cparams.isEmpty) Seq(add)
          else (cparams.head ++ add) +: cparams.tail
        }
        val body1 = withReservedTypeNames(TypeName("Self"))(body : _*) {
          val selfType = q"override type Self = $name";
          val replaceDefs = fieldsByTrait map { case (t, fields) =>
            val replaceName = {
              val TypeName(n) = t.typeSymbol.name
              TermName(replacePrefix + n)
            }
            val copyParams = fields map { f => q"${f.name} = ${f.name}" }
            val params = fields map { f => q"${Modifiers(Flag.PARAM)} val ${f.name} : ${f.typeSignature}" }
            q"override def $replaceName(..$params) = copy(..$copyParams)"
          }
          replaceDefs ++: selfType +: body
        }

        val main = {
          q"$mods class $name[..$tp] $cmods(...$cparams1) extends { ..$early } with ..$parents { $self => ..$body1}"
        }

        c.Expr(q"..${main :: xs}")
      case (x @ q"${mods : Modifiers} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$body }") :: xs =>
        // examples for trait A { val i : Int; val j : String }
        if(tparams.length > 0) {
          throw new NotImplementedError("Generic @fancy traits not supported yet.")
        }
        val decomposeName = TermName(decomposePrefix + name.toString())
        val replaceName = TermName(replacePrefix + name.toString())
        val tname = (name : TypeName).toTermName
        val pname = c.enclosingPackage.symbol //TODO: deprecated
        val companionName = q"$pname.$tname"
        val fields = body collect {   //ex: (i, Int), (j, String)
          case t @ q"$_ val $name : $tpe = $body" if body.isEmpty => (name, tpe)
        }
        val fieldNames = fields map { _._1 }
        val fieldTypes = fields map { _._2 }

        val companion = {
          //COMPANION
          val expectedTpe = tq"${companionTrait.typeSymbol}[$name]"
          (xs match {
            case q"$mods object $cname extends { ..$early } with ..$cparents { $cs => ..$cb }" :: Nil =>
              // If there is already a companion object, we make sure that it extends FancyTraitCompanion
              val nparents = cparents map { case p: c.Tree => c.typecheck(p, c.TYPEmode) } collectFirst {
                case x if x.tpe <:< companionTrait => x
              } match {
                case None => cparents :+ expectedTpe
                case Some(x) =>
                  if (!(x.tpe =:= resolveType(expectedTpe))) {
                    c.error(x.pos, s"Expected: $expectedTpe")
                  }
                  cparents
              }
              q"$mods object $cname extends { ..$early } with ..$nparents { $cs => ..$cb }"
            case Nil =>
              // If there is no companion object, create an empty one.
              q"object $tname extends $expectedTpe"
          }) match {
            case x @ q"$mods object $cname extends { ..$early } with ..$cparents { $cself => ..$cbody }" =>
              // Generate the correct HList-based representation of the new fields.
              val cb1 =
                withReservedTermNames(TermName("Repr"))(cbody : _*) {
                  withReservedTypeNames(TypeName("Repr"))(cbody: _*) {
                    val repr = {
                      val hcons = typeOf[shapeless.::[_,_]].typeSymbol
                      val t = fields.foldRight(tq"${typeTag[HNil].tpe}") { case ((_, h), t) => tq"$hcons[$h, $t]" }
                      q"type Repr = $t"
                    }
                    val createRepr = {
                      val params = fields map { case (name, tpe) => q"${Modifiers(Flag.PARAM)} val $name : $tpe" }
                      val asRepr = fields.foldRight(q"${typeTag[HNil].tpe.typeSymbol.companion}") { case ((x, _), xs) => q"$xs.::($x)" }
                      q"object Repr { def apply(..$params) : Repr = $asRepr }"
                    }
                    val unapply = {
                      val params = fields map { case (name, tpe) => q"${Modifiers(Flag.PARAM)} val $name : $tpe = instance.$name" }
                      val tupleType = tq"(..$fieldTypes)"
                      val asTuple = q"(..${fieldNames map { n => q"instance.$n" }})"
                      q"def unapply[T <: $name](instance : T { type Self <: T }) : Option[($tupleType, Remainder[T])]= Some(($asTuple, instance.$decomposeName))"
                    }
                    repr +: createRepr +: unapply +: cbody
                  }
                }

              val cb2 =
                (findDefnByTypeName(TypeName("Remainder"), cb1 : _*)
                  getOrElse q"class Remainder[+Dividend <: $name](of : Dividend { type Self <: Dividend})") match {
                case q"${mods : Modifiers} class Remainder[+$tp <: $name]($of : ${tp2 : Ident} { type Self <: ${tp3 : Ident}}) extends { ..$edefns } with ..$parents { $ts => ..$tbody }"
                  if (!mods.hasFlag(Flag.ABSTRACT) && tp == tp2.name && tp == tp3.name) =>

                  val tbody1 = withReservedTermNames("$times")(tbody : _*) {
                    val timesWithParams = {
                      val params = fields map { case (name, tpe) => q"${Modifiers(Flag.PARAM)} val $name : $tpe = $of.$name" }
                      q"def *(..$params) : $tp = *(Repr(..$fieldNames))"
                    }
                    val timesRepr = q"def *(right : Repr) : $tp = $of.$replaceName(right)"
                    timesWithParams +: timesRepr +: tbody
                  }
                  q"$mods class Remainder[+$tp <: $name]($of : $tp { type Self <: $tp }) extends { ..$edefns } with ..$parents { $ts => ..$tbody1 }" +:
                  cb1
                case other =>
                  c.error(other.pos,
                    "Remainder is a reserved name for a concrete class with exactly one covariant type parameter X" +
                      s"with an upper bound of $name" +
                      "and one primary constructor with a single argument of that type X { type Self <: X }")
                  cb1
              }
              q"$mods object $cname extends { ..$early } with ..$cparents { $cself => ..$cb2 }"
          }
        }
        //TRAIT
        val main = {
          val body1 =
            withReservedTypeNames(TypeName("Self"))(body : _*) {
              withReservedTermNames(decomposeName, replaceName)(body: _*) {
                val selfType =  q"type Self >: this.type <: $name[..$tparams]" // it is absolutely OBLIGATORY to override this in every descendant
                val decompose = q"lazy val $decomposeName : $companionName.Remainder[Self] = new $companionName.Remainder(this)"
                val replace = {
                  val params = fields map { case (name, tpe) => q"${Modifiers(Flag.PARAM)} val $name : $tpe"}
                  q"def $replaceName(..$params) : Self"
                }
                val replaceRepr = {
                  val hcons = typeOf[shapeless.::.type].termSymbol
                  val pat = fieldNames.foldRight[Tree]( pq"_" ) { (n, t) => pq"$hcons($n,$t)" }
                  q"def $replaceName(newValues : $companionName.Repr) : Self = { newValues match { case $pat => $replaceName(..$fieldNames)}}"
                }
                selfType +: decompose +: replace +: replaceRepr +: body
              }
            }

          val mods1 = { // clear the INTERFACE flag, if any.
            val flags = {
              import Flag._
              Seq(TRAIT, ABSTRACT, FINAL, SEALED, PRIVATE, PROTECTED, LOCAL).foldLeft(NoFlags) {
                (acc, f) =>
                  if(mods.hasFlag(f)) acc | f
                  else acc
              }
            }
            Modifiers(flags, mods.privateWithin, mods.annotations)
          }

          q"$mods1 trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$body1 }"
        }

        c.Expr(q"$main;$companion")
      case x :: xs =>
        c.error(c.enclosingPosition, "The annotation is valid only for case classes and traits")
        val output = x match {
          case _: ValDef | _: TypeDef => xs
          case _ => x :: xs
        }
        c.Expr(q"..$output")
    }
  }
}