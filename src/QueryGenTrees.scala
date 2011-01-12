package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait QueryGenTreeInterface extends QueryInterface{
  def toTree( x: Rep[Any] ):String
}

trait QueryGenTree extends QueryGenBase with QueryGenTreeInterface with QueryExp
{
  def toTree( r: Rep[Any] ):String = renderRep( r, renderTreeDef )
  def renderTreeDef( d : Def[Any] ) : String = d match{
    case Equal ( a,b ) => "Equal( " + toTree( a ) + " , " + toTree( b ) + " )"
    case Plus  ( a,b ) => "Plus( " + toTree( a ) + " , " + toTree( b ) + " )"
    case Minus ( a,b ) => "Minus(" + toTree( a ) + " , " + toTree( b ) + " )"
    case Concat( a,b ) => "Concat( " + toTree( a ) + " , " + toTree( b ) + " )"
    case c: Column[_]  => "Column( " + c.table + "." + c.name + " )"
  }
}
