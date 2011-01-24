package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

import Predef.{any2stringadd => _, _} // prohibit automatic toString conversion of objects when + method is used

object siq {
  // this exposes the query only in terms of its Rep interface, not the actual Exp's because the type is IQueryDsl
  val dsl : IQueryDsl with generators.IFerry with generators.ITree with generators.ISql = new QueryDsl with generators.Ferry with generators.Tree with generators.Sql
  import dsl.{unit => _, _}
  implicit def unitBoolean( v: Boolean ) = dsl.unit(v)
  implicit def unitString( v: String ) = dsl.unit(v)
  implicit def unitDouble( v: Double ) = dsl.unit(v)
  implicit def unitInt( v: Int ) = dsl.unit(v)
  import tables._ // get employee, workgroup, etc. in scope

  def main( args:Array[String] ) {
    printDsl( (7.0:Rep[Double]) + 7.0 )
    printDsl( dsl.unit(7.0) + 7.0 )
    printDsl( dsl.unit(7.0) == "Chris" )
    printDsl( (dsl.unit(7.0):Rep[Double]) + (7:Rep[Double]) == dsl.unit(9) + "Chris" )
    printDsl( for( e <- employee ) yield "test" )
    printDsl( for( e <- employee ) yield e.name )
    printDsl( for( e <- employee; f <- employee ; if e.name == f.name && e.id <> f.id ) yield e.id + " has same name as " + f.id  )
    printDsl( for( e <- employee; w <- workgroup ; if e.workgroup_id == w.id ) yield e.name + " is in group " + w.name )
    printDsl( 7 + 7 == (9:Rep[Double]) )
/*    printDsl( employee.name == "Chris" )
    printDsl( people.age == 5 )
    printDsl( 27 - people.age == 0 )
*/
  }
  /*
  def printInternals( query : Rep[Any] ){
    println( "symbol: " + query)
    // breaking out of dsl mode
    val exposedDsl = dsl.asInstanceOf[BaseExp]
    println( "findDefinition(...): " + exposedDsl.findDefinition(query.asInstanceOf[exposedDsl.Sym[_]]) )
  }*/
  def printDsl( rep:Rep[_] ) {
    println("-----------------------------------------------------")
    println("Expression tree:")
    printTree( rep )
    println("")
    println("Ferry:")
    printFerry( rep )
    println("")
    println("Sql:")
    printSql( rep )
  }
}

// where does this comment come from?
	  // use one of the approaches on http://stackoverflow.com/questions/1252915/scala-how-to-define-generic-function-parameters
	  // to support integer, etc. addition
