package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

import Predef.{any2stringadd => _, _} // prohibit automatic toString conversion of objects when + method is used

object siq {
  // this exposes the query only in terms of its Rep interface, not the actual Exp's because the type is QueryInterface
	val dsl : QueryGenSqlInterface with QueryGenTreeInterface = new QueryGenSql with QueryGenTree{}
  import dsl._

  def main(args : Array[String]) : Unit = {
    val query = unit(7.0) + 7 == unit(9) + "Chris" // entering dsl mode early

    printExample( query )
    printInternals( query )

    printExample( 7 + 7 == (9:Rep[Double]) )
    printExample( col2rep(people.name) == unit("Chris") )
    printExample( 27 - col2rep(people.age) == 0 )
  }

  var i = 0
  def printExample( query : Rep[Any] ){
    i += 1
    println( "\n\nEXAMPLE " + i)
    println( "toSql:  " + toSql(query) )
    println( "toTree: " + toTree(query) )
  }
  def printInternals( query : Rep[Any] ){
    println( "symbol: " + query)

    // breaking out of dsl mode
    val exposedDsl = dsl.asInstanceOf[BaseExp]
    println( "findDefinition(...): " + exposedDsl.findDefinition(query.asInstanceOf[exposedDsl.Sym[_]]) )
  }
}

// where does this comment come from?
	  // use one of the approaches on http://stackoverflow.com/questions/1252915/scala-how-to-define-generic-function-parameters
	  // to support integer, etc. addition
