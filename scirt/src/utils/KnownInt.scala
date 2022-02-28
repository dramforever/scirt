package scirt.utils

import scala.compiletime.*
import scala.compiletime.ops.int.*

trait KnownInt[N <: Int]:
  def getKnownInt: N

object KnownInt:
  def apply[A <: Int](using ki: KnownInt[A]): A = ki.getKnownInt

object knownIntInstances:
  inline given [A <: Int]: KnownInt[A] =
    new KnownInt[A]:
      def getKnownInt: A = constValue[A]

  given [A <: Int, B <: Int](using ka: KnownInt[A], kb: KnownInt[B]): KnownInt[A + B] with
    def getKnownInt: A + B =
      (ka.getKnownInt + kb.getKnownInt).asInstanceOf[A + B]

export knownIntInstances.given
