package scala.virtualization.lms
package epfl
package siq
package generators

import common._
import internal._

trait GeneratorBase extends IQueryDsl
{
  private var current_depth = -1;
  def reset_indentation { current_depth = -1 }
  def indent = " " * current_depth
  def renderRep( r: Rep[Any],
                 renderDef   : Def[Any]   => String,
                 renderConst : Const[Any] => String = renderConst
  ) : String = {
    current_depth += 1
    val returnme = r match {
      case s@Sym(id) => findDefinition(s) match{
        case Some(TP(_,x)) => renderDef( x )
        case _ => throw new Exception()
      }
      case c@Const(_)  => renderConst(c.x)
      case _ => "not a symbol or const"
    }
    current_depth -= 1
    returnme
  }
  def renderConst[T]( c : Const[T] ) = c.x match {
    case c: Char    => '"' + c.toString + '"'
    case s: String  => '"' + s.toString + '"'
    case d: Double  => d.toString
    case i: Int     => i.toString
    case b: Boolean => "1"
    case a : AnyRef => "invalid const " + a.getClass.toString
    case _ => "invalid const "
    //case c: Column[_] => c.table + "." + c.name

    // ... cannot use AnyVal here...
  }
}