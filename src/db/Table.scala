package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait Table extends Base{
  trait PeopleClass{
    val name: Rep[String]
    val age:  Rep[Double]
  }
  def people : PeopleClass
}
trait TableExp extends Table with BaseExp{
  case class Column[T](
    table: String,
    name : String
  ) extends Def[T]
  class myPeopleClass extends PeopleClass{
    val name: Rep[String] = Column[String]( "person","name")
    val age:  Rep[Double]  = Column[Double]( "person","age")
  }
  def people = new myPeopleClass
}
