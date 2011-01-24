package scala.virtualization.lms
package epfl
package siq
package generators

import common._
import internal._

trait IFerry extends IQueryDsl{
  def genFerry( x: Rep[_] ):String
  def printFerry( r:Rep[_] ){ println( genFerry(r) ) }
}
trait Ferry extends GeneratorBase with IFerry with QueryDsl
{
  def genFerry( r: Rep[_] ):String = toFerry(r)
  def genFerry( q:QueryExp ):String = toFerry(q)
  private def toFerry( r: Rep[_] ):String = renderRep( r, renderFerryDef )
  private def toFerry( q:QueryExp ) : String = {//"test" ; def none(q:QueryExp) = {
    def tables2string( tables: List[TableExp[SchemaBase]] ) = tables.map( table => table.alias + " in " + table2string(table) ).mkString(", ")
    def table2string( table: TableExp[SchemaBase] )
      = table.name + " (" + table.columns.map(
                                               c => c.name + " " + c.type_.toLowerCase
                                               ).mkString(", ") + ")"
    q.filter match {
      case Const(true) => "for " + tables2string(q.tables) +  " return " + toFerry(q.projection)
      case _           => "filter(\n  " +toFerry(q.filter)+  ",\n  for " +tables2string(q.tables)+ " " + "\n    return " + toFerry(q.projection) +"\n)"
    }
  }

  def renderFerryDef( d : Def[Any] ) : String = d match{
    case Not ( a )   => "(not "+toFerry(a)+")"
    case And ( a,b ) => "(" + toFerry( a ) + " and " + toFerry( b ) + ")"
    case Or ( a,b )  => "(" + toFerry( a ) + " or " + toFerry( b ) + ")"
    case Equal ( a,b ) => "(" + toFerry( a ) + " == " + toFerry( b ) + ")"
    case Plus  ( a,b ) => "(" + toFerry( a ) + " + " + toFerry( b ) + ")"
    case Minus ( a,b ) => "(" + toFerry( a ) + " - " + toFerry( b ) + ")"
    case ToString ( s ) => toFerry( s )
    case Concat( a,b ) => "(CONCAT(" + toFerry( a ) + "," + toFerry( b ) + "))"
    case c: Column[_]  => c.table.alias + "." + c.name
    case q:QueryExp => toFerry(q)
    case _ => throw new Exception(d.getClass.toString)
  }
}
