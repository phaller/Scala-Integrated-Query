package scala.virtualization.lms
package epfl
package siq

import common._

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack

trait Equal extends Base with OverloadHack {
  def __equal(a: Rep[Any], b: Rep[Any]): Rep[Boolean]
  // == is defined on Any, so no implicits will be used, so the following is necessary
  def __equal(a: Rep[Any], b: Any)(implicit o: Overloaded1): Rep[Boolean] = __equal(a, unit(b))
  def __equal(a: Any, b: Rep[Any])(implicit o: Overloaded2): Rep[Boolean] = __equal(unit(a), b)
}

trait EqualExp extends Equal with BaseExp {
  case class Equal(a: Exp[Any], b: Exp[Any]) extends Def[Boolean]
  def __equal(a: Rep[Any], b: Rep[Any]): Rep[Boolean] = Equal(a,b)
}

trait ScalaGenEqual extends ScalaGenBase with EqualExp {

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, quote(a) + "==" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
