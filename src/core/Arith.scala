package scala.virtualization.lms
package epfl
package siq

import common._

import java.io.PrintWriter

trait Arith[T] extends Base {
  def infix_+(x: Rep[T], y: Rep[T]): Rep[T]
  def infix_-(x: Rep[T], y: Rep[T]): Rep[T]
  def infix_*(x: Rep[T], y: Rep[T]): Rep[T]
  def infix_/(x: Rep[T], y: Rep[T]): Rep[T]
}

trait ArithExp[T] extends Arith[T] with BaseExp {
  case class Plus(x: Exp[T], y: Exp[T]) extends Def[T]
  case class Minus(x: Exp[T], y: Exp[T]) extends Def[T]
  case class Times(x: Exp[T], y: Exp[T]) extends Def[T]
  case class Div(x: Exp[T], y: Exp[T]) extends Def[T]

  def infix_+(x: Exp[T], y: Exp[T]) = Plus(x, y)
  def infix_-(x: Exp[T], y: Exp[T]) = Minus(x, y)
  def infix_*(x: Exp[T], y: Exp[T]) = Times(x, y)
  def infix_/(x: Exp[T], y: Exp[T]) = Div(x, y)
}

trait ScalaGenArith[T] extends ScalaGenBase with ArithExp[T] {
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}