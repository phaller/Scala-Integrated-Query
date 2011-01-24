package scala.virtualization.lms
package epfl
package siq

import common._

import java.io.PrintWriter

trait BooleanOperators extends Base {
  //def infix_==(x: Rep[Any], y: Rep[Any]): Rep[Boolean]
  def infix_&&(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean]
  def infix_||(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean]
}

trait BooleanExp extends BooleanOperators with BaseExp {
  //case class Equals(x: Exp[Any], y: Exp[Any]) extends Def[Boolean]
  case class And(x: Exp[Boolean], y: Exp[Boolean]) extends Def[Boolean]
  case class Or(x: Exp[Boolean], y: Exp[Boolean]) extends Def[Boolean]

  //def infix_==(x: Rep[Any], y: Rep[Any]) = Equals(x, y)
  def infix_&&(x: Rep[Boolean], y: Rep[Boolean]) = And(x, y)
  def infix_||(x: Rep[Boolean], y: Rep[Boolean]) = Or(x, y)
}

trait ScalaGenBoolean extends ScalaGenBase with BooleanExp {
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    //case Equals(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case And(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Or(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}