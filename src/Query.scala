package scala.virtualization.lms
package epfl
package siq

import common._
import internal._

trait QueryInterface extends
         QueryOperators
    with Arith[Double]
    with LimitedEqual
    with StringOps
    with Table

trait QueryExp extends
         QueryInterface
    with QueryOperatorsExp
    with ArithExp[Double]
    with StringOpsExp
    with LimitedEqualExp
    with TableExp
