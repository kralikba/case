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

    val util = ContextUtil[c.type](c)
    import util._

    val companionTrait = typeOf[FancyTraitCompanion[_]]

    annottees.map(_.tree).toList match { // excessive type annotations are here to make the IntelliJ plugin a bit less grumpy. Will be removed in a future version.
      case q"${mods: Modifiers} class ${name : TypeName}[..$tp] $cmods(...${_cparamss}) extends { ..$early } with ..$parents { $self => ..$body }" :: xs
        if (mods.hasFlag(Flag.CASE)) =>
        val cparamss : Seq[Seq[Tree]] = _cparamss

        val cparamsstail = {
          if(cparamss.isEmpty) Seq[Seq[Tree]]()
          else cparamss.tail
        }

        val (caseparams, caseparamsWithDefault) = cparamss
          .headOption.getOrElse(Seq[Tree]())
          .partition { case q"$_ val $_ : $_ = $rhs" => rhs == EmptyTree }

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
          val linearized = linearize(ptpe, List()) filter { t =>
            if(t.companion =:= NoType) false
            else t.companion <:< companionTrait
          }

          // Check if the direct parents appear in the same order as their linearization order.
          // Emit a warning, if not.
          {
            val fancyParents = ptpe filter { linearized contains _ }
            val fancyParentsOrd = linearized filter { fancyParents contains _ }
            if (fancyParents != fancyParentsOrd) {
              c.warning(parents.head.pos,
                s"@fancy supertypes do not appear in their linearization order. " +
                  s"This might cause confusion regarding $name's constructor's, $name.apply's and $name.fromComponent's parameter order. " +
                  s"Their correct ordering is: ${fancyParentsOrd.mkString(" with ")}.")
            }
          }
          linearized
        }

        val fieldsByTrait = fancyTraits map { t =>
          val params = t
            .companion
            .member(TermName("Repr")).typeSignature
            .member(TermName("apply")).typeSignature
            .paramLists.head
          val fields = params map { _.asTerm }
          val defaults = fields.zipWithIndex.map { _ match {
              case (sym, ord) if sym.isParamWithDefault =>
                //TODO: overriding default values - how should that work at all?
                Some[Tree](q"${t.typeSymbol.companion}.Repr.${TermName("apply$default$" + (ord+1).toString)}")
              case _ => None
            }
          }
          (t, fields, defaults)
        }

        val cparams1 : Seq[Seq[Tree]] = {
          val all = fieldsByTrait.flatMap { t => t._2 zip t._3 }
          val mandatory : Seq[Tree] = all.collect { case (sym, None) =>
            q"val ${sym.name.toTermName} : ${sym.typeSignature}"
          }
          val optional : Seq[Tree] = all.collect { case (sym, Some(default)) =>
            q"override val ${sym.name.toTermName} : ${sym.typeSignature} = $default"
          }
          (caseparams ++ mandatory ++ caseparamsWithDefault ++ optional) +: cparamsstail
        }

        val companion = {
          xs.headOption.getOrElse(q"object ${name.toTermName}") match {
            case q"$mods object $cname extends { ..$early } with ..$cparents { $cs => ..$cb }" =>
              val cb1 = withReservedTermNames("fromComponents")(cb : _*) {
                val fromComponents = {
                  val ts = fieldsByTrait map { case (tpe, fields, defaults) =>
                    (tpe, fields zip defaults, tpe.typeSymbol.name.toTermName)
                  }

                  val formalParamss = {
                    (caseparams ++
                      ts.map { case (tpe, _, name) => q"$modParam val $name : $tpe" } ++
                      caseparamsWithDefault) +:
                    cparamsstail
                  }

                  val concreteParamss = {
                    val (withDefaults, withoutDefaults) = ts
                      .flatMap { case (_, fields, tpeName) =>
                        fields map { case (fn,default) => (q"$tpeName.${fn.name}", default) }
                      }
                      .partition(_._2.isDefined)

                    (caseparams.map(valdefToIdent) ++
                      withoutDefaults.map(_._1) ++
                      caseparamsWithDefault.map(valdefToIdent) ++
                      withDefaults.map(_._1)) +:
                    (cparamsstail map { _ map valdefToIdent})
                  }

                  q"def fromComponents(...$formalParamss) : $name = new $name(...$concreteParamss)"
                }
                fromComponents +: cb
              }
              q"$mods object $cname extends { ..$early } with ..$cparents { $cs => ..$cb1 }"
          }
        }

        val main = {
          val body1 = withReservedTypeNames(TypeName("Self"))(body : _*) {
            val selfType = q"override type Self = $name";
            val replaceDefs = fieldsByTrait map { case (t, fields, defaults) =>
              val replaceName = {
                val TypeName(n) = t.typeSymbol.name
                TermName(replacePrefix + n)
              }
              val formalParams = fields map { f => q"$modParam val ${f.name} : ${f.typeSignature} = ${f.name}"}
              val concreteParams = fields map { f => q"${f.name} = ${f.name}" }
              q"override def $replaceName(..$formalParams) = copy(..$concreteParams)"
            }
            replaceDefs ++: selfType +: body
          }

          q"$mods class $name[..$tp] $cmods(...$cparams1) extends { ..$early } with ..$parents { $self => ..$body1}"
        }

        c.Expr(q"$main;$companion")
      case (x @ q"${_mods} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$body }") :: xs =>
        // examples for trait A { val i : Int; val j : String }
        val mods = _mods.asInstanceOf[Modifiers]
        if(tparams.length > 0) {
          throw new NotImplementedError("Generic @fancy traits not supported yet.")
        }

        val decomposeName = TermName(decomposePrefix + name.toString())
        val replaceName = TermName(replacePrefix + name.toString())
        val equalsInName = TermName("equalsIn" + name.toString())
        val tname = (name : TypeName).toTermName
        val pname = c.enclosingPackage.symbol //TODO: deprecated
        val companionName = q"$pname.$tname"

        val fields = {
          val vals = body collect {
            case v @ q"$mods val $_  : $_ = $rhs" if mods.hasFlag(Flag.ABSTRACT) || rhs.isEmpty => v.asInstanceOf[ValDef]
          }

          // Check that the abstract vals having default values are not succeeded by ones not having any.
          // If this is false, issue a warning.
          vals.dropWhile( _.rhs.isEmpty ).collectFirst {
            case v if v.rhs.isEmpty =>
              c.error(v.pos, "Optional fields must never precede any mandatory ones.")
          }

          vals map {
            // (name, tpe, defaultValue : Tree)
            case q"$_ val $name : $tpe" => (name, tpe, None)
            case q"$_ val $name : $tpe = $rhs" => (name, tpe, Some(rhs))
          }
        }

        val body0 = body map { // abstract vals with values are disallowed
          case q"$mods val $n : $t = $v" if mods.hasFlag(Flag.ABSTRACT) => q"${mods &~ Flag.ABSTRACT} val $n : $t = $v"
          case x => x
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
                      val t = fields.foldRight(tq"${typeTag[HNil].tpe}") { case ((_, h, _), t) => tq"$hcons[$h, $t]" }
                      q"type Repr = $t"
                    }
                    val createRepr = {
                      val params = {
                        (fields collect { case (name, tpe, None) => q"$modParam val $name : $tpe" }) ++
                          (fields collect { case (name, tpe, Some(default)) => q"$modParam val $name : $tpe = $default" })
                      }
                      val asRepr = fields.foldRight(q"${typeTag[HNil].tpe.typeSymbol.companion}") { case ((x, _, _), xs) => q"$xs.::($x)" }
                      q"object Repr { def apply(..$params) : Repr = $asRepr }"
                    }
                    val unapply = {
                      val params = fields map { case (name, tpe, _) => q"$modParam val $name : $tpe = instance.$name" }
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
                      val params = fields map { case (name, tpe, _) => q"${Modifiers(Flag.PARAM)} val $name : $tpe = $of.$name" }
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
            withReservedTypeNames(TypeName("Self"))(body0 : _*) {
              withReservedTermNames(decomposeName, replaceName)(body0: _*) {
                val selfType =  q"type Self >: this.type <: $name[..$tparams]" // it is absolutely OBLIGATORY to override this in every descendant
                val decompose = q"lazy val $decomposeName : $companionName.Remainder[Self] = new $companionName.Remainder(this)"
                val replace = {
                  val params = fields map { case (name,tpe, _) => q"$modParam val $name : $tpe = $name"}
                  q"def $replaceName(..$params) : Self"
                }
                val replaceFrom = {
                  val params = fields map { case (name, _, _) => q"$name = from.$name" }
                  q"def $replaceName(from : $name) : Self = $replaceName(..$params)"
                }
                val replaceRepr = {
                  val hcons = typeOf[shapeless.::.type].termSymbol
                  val pat = fieldNames.foldRight[Tree]( pq"_" ) { (n, t) => pq"$hcons($n,$t)" }
                  q"def $replaceName(newValues : $companionName.Repr) : Self = { newValues match { case $pat => $replaceName(..$fieldNames)}}"
                }
                val equalsIn = {
                  val expr = fieldNames.foldLeft(q"true" : Tree) { case (e, name) => q"(other.$name == $name) && $e" }
                  q"def $equalsInName(other : $name) : Boolean = $expr"
                }
                selfType +: decompose +: replace +: replaceRepr +: replaceFrom +: equalsIn +: body0
              }
            }

          val mods1 = mods &~ Flag.INTERFACE

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