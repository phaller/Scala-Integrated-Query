package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait SchemataBase extends BaseExp{
  trait SchemaBase
  trait Query

  abstract class Table[+T <: SchemaBase](
    val name   : String,
    val alias_index : Int
  ){
    def alias : String
    val schema : T
    var filter : Rep[Boolean]
    def map( f : T => Rep[String] ) : Rep[Query]
    def flatMap( f : T => Rep[Query] ) : Rep[Query]
    def withFilter( f : T => Rep[Boolean] ) : Table[T]
  }
}

trait SchemataBaseExp extends SchemataBase with BaseExp{
  abstract class TableExp[+T <: SchemaBase](
    name   : String,
    alias_index : Int
  ) extends Table[T](name, alias_index){
    //val _table : Table[T] = this
    val columns : Array[Column[_]]
    def alias = name(0).toLower + alias_index.toString
    var filter : Rep[Boolean] = true
    def map( f : T => Rep[String] ) = toAtom( // FIXME: why dows this need to be called explcitely?
      new QueryExp(
        List[TableExp[SchemaBase]](this)
        , f( schema ), filter
      )
    )
    def flatMap( f : T => Rep[Query] ) = {
      val query =  f( schema )
      query match { // FIXME ... can we do this statically typed?
        case s@Sym(id) => findDefinition(s) match{
          case Some(TP(_,q:QueryExp)) => {
            q.tables = this :: q.tables
            toAtom(q)
          }
          case _ => throw new Exception(s.toString)
        }
        case c@Const(_)  => throw new Exception(c.toString)
        case _ => throw new Exception()
      }
    }
    def withFilter( f : T => Rep[Boolean] ) = {
      filter = f( schema )
      this
    }
  }

  case class Column[T](
    table: Table[SchemaBase],
    name : String = null,
    type_ : String = null
  ) extends Def[T]

  class QueryExp (
    var tables : List[TableExp[SchemaBase]],
    val projection : Rep[String],
    val filter : Rep[Boolean] = true
  ) extends Def[Query] with Query
}
