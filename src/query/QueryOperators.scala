package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait QueryOperators extends Base {
  def where( predicate: Rep[Boolean] ):Rep[QueryOperators]
}

trait QueryOperatorsExp extends QueryOperators with BaseExp {
  case class Where_(predicate: Exp[Boolean]) extends Def[QueryOperators]
  def where( predicate: Exp[Boolean] ) = Where_(predicate)
}