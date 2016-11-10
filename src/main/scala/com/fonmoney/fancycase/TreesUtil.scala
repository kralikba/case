package com.fonmoney.fancycase

import scala.reflect.api.{Trees, Universe}
import scala.reflect.macros.whitebox.Context
/**
  * Created by kralikba on 2016.11.03..
  */
case class TreesUtil[C <: Context](context : C) {
  import context.universe._

  //HACK; what is the idiomatic way to resolve a type?
  def resolveType(t : Tree) : Type = {
    context.typecheck(q"??? : $t").tpe
  }

  def withReservedTermNames(names : TermName*)(in : Tree*)(f : => Seq[Tree]) = {
    names.flatMap(findDefnByTermName(_, in : _*)) match {
      case Seq() => f
      case ns =>
        for(n <- ns) { context.error(n.pos, s"${n.toString()} is a reserved name") }
        in
    }
  }

  //TODO: Unit tests for findByTermName
  def findDefnByTermName(name : TermName, defns : Tree*) : Option[Tree] = {
    def defTermName(t : Tree) : Seq[TermName] = {
      t match {
        case q"$_ val ${n: TermName} : $_ = $_" => Seq(n)
        case q"$_ var ${n: TermName} : $_ = $_" => Seq(n)
        case q"$_ def ${n: TermName} : $_ = $_" => Seq(n)
        case q"$_ object ${n: TermName} extends { ..$_ } with ..$_ { $_ => ..$_ }" => Seq(n)
        case q"$_ val ${pat : Tree}: $_ = $_" => patTermName(pat)
        case q"$_ var ${pat : Tree}: $_ = $_" => patTermName(pat)
        case _ => Seq.empty
      }
    }

    def patTermName(t : Tree) : Seq[TermName] = {
      t match {
        case pq"${n : TermName} @ ${pat : Tree}" => n +: patTermName(pat)
        case pq"$x | ..$xs" => (x +: xs) flatMap patTermName
        case pq"$_(..$pats)" => pats flatMap patTermName
        case _ => Seq()
      }
    }

    defns zip (defns map defTermName) collectFirst {
      case (t, ns) if ns.contains(name) => t
    }
  }

  def withReservedTypeNames(names : TypeName*)(in : Tree*)(f : => Seq[Tree]) = {
    names.flatMap(findDefnByTypeName(_, in : _*)) match {
      case Seq() => f
      case ns =>
        for(n <- ns) { context.error(n.pos, s"${n.toString()} is a reserved name") }
        in
    }
  }

  def findDefnByTypeName(name : TypeName, defns: Tree*) : Option[Tree] = {
    val defTypeName : PartialFunction[Trees#Tree, TypeName]= {
      case q"$_ type ${n : TypeName}[..$_] = $_" => n
      case q"$_ class ${n : TypeName}[..$_] $_ (...$_) extends { ..$_ } with ..$_ { $_ => $_ }" => n
      case q"$_ trait ${n : TypeName}[..$_] extends { ..$_ } with ..$_ { $_ => $_ }" => n
    }

    defns zip (defns map (defTypeName.lift)) collectFirst {
      case (t, Some(n)) if n == name => t
    }
  }

  def valdefToName(t : Tree) : TermName = {
    t match {
      case q"$_ val $n : $_ = $_" => n
    }
  }

  def valdefToIdent(t : Tree) = {
    q"${valdefToName(t)}"
  }
}
