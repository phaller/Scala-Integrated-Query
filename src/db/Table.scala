package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait Table extends Base{
  case class Column[T](
    table: String,
    name : String
  )
  trait PeopleClass{
    val name: Column[String]
    val age:  Column[Double]
  }
  def people : PeopleClass
  implicit def col2rep[T]( c: Column[T] ) : Rep[T]
}
trait TableExp extends Table with BaseExp{
  case class ColumnDef[T]( c:Column[T] ) extends Def[T]
  class myPeopleClass extends PeopleClass{
    val name = Column[String]( "person","name")
    val age  = Column[Double]( "person","age")
  }
  def people = new myPeopleClass
  implicit def col2rep[T]( c: Column[T] ) : Rep[T] = ColumnDef[T](c)
}