package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait QueryGenSqlInterface extends QueryInterface{
  def toSql( x: Rep[Any] ):String
}
trait QueryGenSql extends QueryGenBase with QueryGenSqlInterface with QueryExp
{
  def toSql( r: Rep[Any] ):String = renderRep( r, renderSqlDef )
  def renderSqlDef( d : Def[Any] ) : String = d match{
    case Equal ( a,b ) => "(" + toSql( a ) + " == " + toSql( b ) + ")"
    case Plus  ( a,b ) => "(" + toSql( a ) + " + " + toSql( b ) + ")"
    case Minus ( a,b ) => "(" + toSql( a ) + " - " + toSql( b ) + ")"
    case Concat( a,b ) => "(CONCAT(" + toSql( a ) + "," + toSql( b ) + "))"
    case c: Column[_]  => c.table + "." + c.name
  }
}
