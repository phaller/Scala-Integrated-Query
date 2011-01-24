package scala.virtualization.lms
package epfl
package siq
package generators

import scala.reflect

import common._
import internal._

trait ITree extends IQueryDsl{
  def genTree( x: Rep[Any] ):String
  def printTree( r:Rep[_] ) = println( genTree(r) )
}

trait Tree extends GeneratorBase with ITree with QueryDsl
{
  def genTree( r:Rep[_] ) = toTree(r)
  def node2string( name:String, args:List[Rep[Any]] ) = indent + name + "(\n" +
                                                        indent + " " + args.map( toTree ).mkString( ",\n " + indent ) + "\n" +
                                                        indent + ")"
  def node2string( name:String, arg:Rep[Any] ) = name + "( " + toTree(arg) + " )"
  case class Literal( s:String ) extends Def[String]
  def literal(s:String) : Rep[String] = Literal( s )

  private def toTree( r: Rep[Any] ) : String = renderRep( r, renderTreeDef )
  def renderTreeDef( d : Def[Any] ) : String = d match{
    case Not ( a )   => node2string( "Not", a )
    case And ( a,b )   => node2string( "And", List(a, b) )
    case Or ( a,b )    => node2string( "Or", List(a, b) )
    case Equal ( a,b ) => node2string( "Equal", List(a, b) )
    case Plus  ( a,b ) => node2string( "Plus", List(a, b) )
    case Minus ( a,b ) => node2string( "Minus", List(a, b) )
    case Concat( a,b ) => node2string( "Concat", List(a, b) )
    case c: Column[_]  => node2string( "Column", literal(c.table.alias+"."+c.name) )
    case ToString(s)   => node2string( "ToString", s )
    case e:QueryExp    => node2string( "Query", List( literal("List( Table(%s) )".format(e.tables.map(t=>t.name+","+t.alias).mkString("), Table("))), e.projection, e.filter ) )
    case Literal(s)    => s
    case _ => throw new Exception(d.getClass.toString)
  }
}


/*trait ITreeGenerator extends IQueryDsl{
  def genTree( r: Rep[Any] ):String
}

trait TreeGenerator extends QueryDsl with ITreeGenerator
{
  def genTree( r: Rep[Any] ) = toTree(r)
  private def toTree( r: Rep[Any] ):String = renderRep( r, renderTreeDef )
  private def renderTreeDef( d : Def[Any] ) : String = d match{
    case Equal ( a,b ) => "Equal( " + toTree( a ) + " , " + toTree( b ) + " )"
    case Plus  ( a,b ) => "Plus( " + toTree( a ) + " , " + toTree( b ) + " )"
    case Minus ( a,b ) => "Minus(" + toTree( a ) + " , " + toTree( b ) + " )"
    case Concat( a,b ) => "Concat( " + toTree( a ) + " , " + toTree( b ) + " )"
    case c: Column[_]  => "Column( " + c.table + "." + c.name + " )"
    //case Select(select,from) => "Select( " + toTree( select ) + ", " + toTree( from ) + " )"
  }
}
*/