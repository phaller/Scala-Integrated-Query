package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait QueryGenBase extends QueryExp
{
		def renderRep( r: Rep[Any],
                   renderDef   : Def[Any]   => String,
                   renderConst : Const[Any] => String = renderConst
    ) : String = {
			r match {
				case s@Sym(id) => findDefinition(s) match{
					case Some(TP(_,x)) => renderDef( x )
					case _ => throw new Exception()
				}
				case c@Const(_)  => renderConst(c.x)
			}
		}
    def renderConst[T]( c : Const[T] ) = c.x match {
      case c: Char   => '"' + c.toString + '"'
      case s: String => '"' + s.toString + '"'
      case d: Double => d.toString
      case i: Int    => i.toString
      //case c: Column[_] => c.table + "." + c.name

      // ... cannot use AnyVal here...
    }
	}