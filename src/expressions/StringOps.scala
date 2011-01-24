package scala.virtualization.lms
package epfl
package siq
package expressions

import common._

import scala.virtualization.lms.util.OverloadHack

trait IStringOperations extends Base with OverloadHack {
  def infix_+( l:Rep[String], r:Rep[String] )(implicit x: Overloaded1): Rep[String]
  //implicit def int2RepString( i:Int ) : Rep[String] = unit(i.toString)
  implicit def int2RepString( i: Rep[Any] ) : Rep[String]
  /*def infix_+( l:Rep[Any], r:Rep[String] )(implicit x: Overloaded2): Rep[String]
  def infix_+( l:Rep[String], r:Rep[Any] )(implicit x: Overloaded3): Rep[String]*/
}

trait StringOperations extends IStringOperations with BaseExp{
  case class Concat(x: Exp[String], y: Exp[String]) extends Def[String]
  case class ToString(x: Exp[Any]) extends Def[String]
  implicit def int2RepString( i: Rep[Any] ) : Rep[String] = ToString(i)
  def stringify( a : Any ) : Const[String] = a match{
	  case Const(value) => Const(value.toString)
	  case _ => Const(a.toString)
  }
  def infix_+( l:Exp[String], r:Exp[String] )(implicit x: Overloaded1): Rep[String] = Concat(l, r)
  /*def infix_+( l:Exp[Any], r:Exp[String] )(implicit x: Overloaded2): Rep[String] = Concat(stringify(l), r)
  def infix_+( l:Exp[String], r:Exp[Any] )(implicit x: Overloaded3): Rep[String] = Concat(l, stringify(r))*/
}