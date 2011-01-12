package scala.virtualization.lms
package epfl
package siq

import common._

import scala.virtualization.lms.util.OverloadHack

trait StringOps extends Base with OverloadHack {
  def infix_+( l:Rep[String], r:Rep[String] )(implicit x: Overloaded1): Rep[String]
  def infix_+( l:Rep[AnyVal], r:Rep[String] )(implicit x: Overloaded2): Rep[String]
  def infix_+( l:Rep[String], r:Rep[AnyVal] )(implicit x: Overloaded3): Rep[String]
}

trait StringOpsExp extends StringOps with BaseExp{
  case class Concat(x: Exp[String], y: Exp[String]) extends Def[String]
  def stringify( a : Any ) : Const[String] = a match{
	  case Const(value) => Const(value.toString)
	  case _ => Const(a.toString)
  }
  def infix_+( l:Exp[String], r:Exp[String] )(implicit x: Overloaded1): Rep[String] = Concat(l, r)
  def infix_+( l:Exp[AnyVal], r:Exp[String] )(implicit x: Overloaded2): Rep[String] = Concat(stringify(l), r)
  def infix_+( l:Exp[String], r:Exp[AnyVal] )(implicit x: Overloaded3): Rep[String] = Concat(l, stringify(r))
}