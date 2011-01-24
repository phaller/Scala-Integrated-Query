package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait IQueryDsl extends
         Arith[Double]
    with LimitedEqual
    with expressions.IStringOperations
    with SchemataBase
    with Schemata
    with BooleanOperators
// TODO: replace this by proper !=
{
  def infix_<>( a:Rep[_], b:Rep[_] ) : Rep[Boolean]
}

trait QueryDsl extends
         IQueryDsl
    with ArithExp[Double]
    with expressions.StringOperations
    with LimitedEqualExp
    with SchemataBaseExp
    with SchemataExp
    with BooleanExp
// TODO: replace this by proper !=
{
  case class Not( r:Rep[Boolean] ) extends Def[Boolean]
  def infix_<>( a:Rep[_], b:Rep[_] ) : Rep[Boolean] = Not( Equal(a,b) )
}
