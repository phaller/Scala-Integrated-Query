package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

/* This file should finally be auto-generated from a database schema */

trait Schemata extends Base with SchemataBase{
  abstract class EmployeeSchemaBase(
    val id: Rep[Int],
    val name: Rep[String],
    val workgroup_id: Rep[Int]
  ) extends SchemaBase
  abstract class WorkgroupSchemaBase(
    val id: Rep[Int],
    val name: Rep[String]
  ) extends SchemaBase
  abstract class Tables{
    def employee : Table[EmployeeSchemaBase]
    def workgroup : Table[WorkgroupSchemaBase]
  }
  val tables : Tables
}

trait SchemataExp extends Schemata with BaseExp with SchemataBaseExp{
  class EmployeeSchema(
    var table : Table[EmployeeSchema]
  ) extends EmployeeSchemaBase(
    Column[Int]   ( table, "id",           "Int" ),
    Column[String]( table, "name",         "String" ),
    Column[Int]   ( table, "workgroup_id", "Int" )
  )
  class WorkgroupSchema(
    var table : Table[WorkgroupSchema]
  ) extends WorkgroupSchemaBase(
        Column[Int]   ( table, "id",   "Int" ),
        Column[String]( table, "name", "String" )
  )
  val tables = new Tables{
    var alias_counter = 0
    def employee = new TableExp[EmployeeSchema]( "Employee", { alias_counter += 1; alias_counter } ){
      val schema = new EmployeeSchema( this )
      val columns = Array[Column[_]](
        Column[Int]   ( this, "id",           "Int" ),
        Column[String]( this, "name",         "String" ),
        Column[Int]   ( this, "workgroup_id", "Int" )
      )
    }
    def workgroup = new TableExp[WorkgroupSchema]( "Workgroup", { alias_counter += 1; alias_counter } ){
      val schema = new WorkgroupSchema( this )
      val columns = Array[Column[_]](
        Column[Int]   ( this, "id",   "Int" ),
        Column[String]( this, "name", "String" )
      )
    }
  }

}
