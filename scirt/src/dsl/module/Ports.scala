package scirt.dsl.module

import scirt.mlir

import scala.quoted.*
import scirt.signal.Hardware
import scirt.dsl.wire.Wire

class Ports

implicit class PortsSelector[P <: Ports](inner: P) extends Selectable:
  inline def selectDynamic(inline field: String)(using mod: Module[P]): Any =
    ${ Ports.selectDynamicImpl[P]('{ inner }, '{ mod }, '{ field }) }

object Ports:
  trait Known[P <: Ports]:
    def has: Map[String, mlir.Type]
    def needs: Map[String, mlir.Type]

  inline given validPorts[P <: Ports]: Known[P] = ${ validPortsImpl[P] }

  case class PortsRepr(
    has: Map[String, Type[? <: Any]],
    needs: Map[String, Type[? <: Any]])

  def portsRepr[P <: Ports : Type](using Quotes): PortsRepr =
    import quotes.reflect.*

    def work(tpe: TypeRepr): Seq[(String, TypeRepr)] =
      tpe match
        case Refinement(tpe, field, ty) =>
          (field, ty) +: work(tpe)
        case _ if tpe =:= TypeRepr.of[Ports] =>
          Seq()

    val (has, needs) =
      work(TypeRepr.of[P]).partitionMap((field, ty) =>
        ty match
          case AppliedType(wireTy, Seq(ty))
            if wireTy =:= TypeRepr.of[Wire] =>
              Right((field, ty.asType.asInstanceOf[Type[? <: Any]]))
          case ty =>
            Left((field, ty.asType.asInstanceOf[Type[? <: Any]])))

    PortsRepr(has.toMap, needs.toMap)

  def selectDynamicImpl[P <: Ports : Type](
    inner: Expr[P], mod: Expr[Module[P]], field: Expr[String]
  )(using Quotes): Expr[Any] =
    val ports = portsRepr[P]
    if ports.has.contains(field.value.get) then
      val hardware = getHardware(ports.has(field.value.get))
      '{ ${ hardware }.fromSignal(${ mod }.getInput(${ field })) }
    else if ports.needs.contains(field.value.get) then
      val ty = ports.needs(field.value.get)
      val hardware = getHardware(ty)
      ty match
        case '[t] =>
          '{
            Wire.fromSignal[t](${ mod }.getOutput(${ field }))
              (using ${ hardware.asInstanceOf }, ${ mod })
          }
    else
      throw RuntimeException("This shouldn't be possible")

  def getHardware(ty: Type[? <: Any])(using Quotes): Expr[Hardware[?]] =
    import quotes.reflect.*

    val tpt = TypeTree.of(using ty)
    val hardwareTpe = Applied(TypeTree.of[Hardware], List(tpt)).tpe

    Implicits.search(hardwareTpe) match
      case iss: ImplicitSearchSuccess => iss.tree.asExprOf[Hardware[?]]
      case isf: ImplicitSearchFailure =>
        report.error(s"${tpt.show} is not a Hardware")
        '{ ??? }

  def validPortsImpl[P <: Ports : Type](using Quotes): Expr[Known[P]] =
    import quotes.reflect.*

    val rep = portsRepr[P]

    def genTuple(field: String, ty: Type[? <: Any]): Expr[(String, mlir.Type)] =
      '{ ${ Expr(field) } -> ${ getHardware(ty) }.underlyingType }

    val has: Seq[Expr[(String, mlir.Type)]] = rep.has.toSeq.map(genTuple)
    val needs: Seq[Expr[(String, mlir.Type)]] = rep.needs.toSeq.map(genTuple)

    val vHas = Varargs(has)
    val vNeeds = Varargs(needs)

    '{
      new Known[P]:
        def has = Map(${ vHas }: _*)
        def needs = Map(${ vNeeds }: _*)
    }
