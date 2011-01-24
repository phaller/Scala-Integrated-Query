package scala.virtualization.lms
package epfl
package siq
package generators

import common._
import internal._

trait ISql extends IQueryDsl{
  def genSql( x: Rep[Any] ):String
  def printSql( x: Rep[Any] )= println(genSql(x))
}
trait Sql extends GeneratorBase with ISql with QueryDsl
{
  def genSql( r:Rep[Any] ) = toSql(r)
  def toSql( r: Rep[Any] ):String = renderRep( r, renderSqlDef )

  def renderSqlDef( d : Def[Any] ) : String = d match{
    case Not ( a )   => "(NOT "+toSql(a)+")"
    case And ( a,b ) => "(" + toSql( a ) + " AND " + toSql( b ) + ")"
    case Or ( a,b )  => "(" + toSql( a ) + " OR " + toSql( b ) + ")"
    case Equal ( a,b ) => "(" + toSql( a ) + " = " + toSql( b ) + ")"
    case Plus  ( a,b ) => "(" + toSql( a ) + " + " + toSql( b ) + ")"
    case Minus ( a,b ) => "(" + toSql( a ) + " - " + toSql( b ) + ")"
    case ToString ( s ) => toSql( s )
    case Concat( a,b ) => "(CONCAT(" + toSql( a ) + "," + toSql( b ) + "))"
    case c: Column[_]  => c.table.alias + "." + c.name
    case q:QueryExp => "SELECT %s \nFROM   %s \nWHERE  %s" format( toSql(q.projection), q.tables.map(t=>t.name+" "+t.alias).mkString(", "), toSql(q.filter) )
  }
}
