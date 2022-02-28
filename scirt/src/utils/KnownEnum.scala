package scirt.utils

import scala.reflect.*
import scala.quoted.*

trait KnownEnum[+E <: Enum]:
  def getKnownEnum: E

object KnownEnum:
  def apply[E <: Enum](using ke: KnownEnum[E]) = ke.getKnownEnum

inline given knownEnumMacro[E <: Enum]: KnownEnum[E] = ${ knownEnumImpl[E] }

def knownEnumImpl[T <: Enum](using Quotes, Type[T]): Expr[KnownEnum[T]] =
  import quotes.reflect.*

  val enumVal =
    TypeRepr.of[T].simplified.dealias match
      case TermRef(parent: TermRef, member) =>
        Select.unique(Ref.term(parent), member).asExprOf[T]
      case tpe =>
        report.error(s"${tpe.show} must be a enum singleton type")
        '{ ??? }

  '{
    new KnownEnum[T]:
      def getKnownEnum: T = ${ enumVal }
  }
