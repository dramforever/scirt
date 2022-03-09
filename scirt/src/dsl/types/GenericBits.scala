package scirt.dsl.types

import scirt.mlir.Type
import scirt.signal.*
import scirt.dsl.ops.*

import Type.Signedness.*
import scirt.utils.{given, *}

// FIXME: When we eventually write higher-level modules, re-export these at the
// user-level modules instead.
export scirt.utils.knownIntInstances.given
export scirt.utils.knownEnumMacro

class GenericBits[W <: Int, Sg <: Type.Signedness](val signal: Signal)

opaque type UInt[W <: Int] = GenericBits[W, Unsigned.type]
opaque type SInt[W <: Int] = GenericBits[W, Signed.type]
opaque type BitVec[W <: Int] = GenericBits[W, Signless.type]

given given_Hardware_GenericBits[W <: Int : KnownInt, Sg <: Type.Signedness : KnownEnum]: Hardware[GenericBits[W, Sg]] with
  def underlyingType = Type.Integer(KnownInt[W], KnownEnum[Sg])
  def fromSignal(signal: Signal ) = GenericBits[W, Sg](signal)
  def toSignal(value: GenericBits[W, Sg]) = value.signal

given [W <: Int : KnownInt]: Hardware[UInt[W]] = given_Hardware_GenericBits
given [W <: Int : KnownInt]: Hardware[SInt[W]] = given_Hardware_GenericBits
given [W <: Int : KnownInt]: Hardware[BitVec[W]] = given_Hardware_GenericBits

given given_Add_GenericBits[W <: Int : KnownInt, Sg <: Type.Signedness : KnownEnum](using cx: Context): Add[GenericBits[W, Sg]] with
  def add(a: GenericBits[W, Sg], b: GenericBits[W, Sg]): GenericBits[W, Sg] =
    import scirt.mlir.*
    import Type.*

    val ty = Hardware.underlyingType[GenericBits[W, Sg]]
    val res = cx.allocate("add")

    cx.add(Operation(
      OperationId("comb.add"),
      Function(Seq(ty, ty), Seq(ty)),
      results = Seq(res.valueId).map(OpResult(_)),
      uses = Seq(a, b).map(v => ValueUse(Hardware.toSignal(v).valueId))))

    Hardware.fromSignal(res)

given given_Sub_GenericBits[W <: Int : KnownInt, Sg <: Type.Signedness : KnownEnum](using cx: Context): Sub[GenericBits[W, Sg]] with
  def sub(a: GenericBits[W, Sg], b: GenericBits[W, Sg]): GenericBits[W, Sg] =
    import scirt.mlir.*
    import Type.*

    val ty = Hardware.underlyingType[GenericBits[W, Sg]]
    val res = cx.allocate("sub")

    cx.add(Operation(
      OperationId("comb.sub"),
      Function(Seq(ty, ty), Seq(ty)),
      results = Seq(res.valueId).map(OpResult(_)),
      uses = Seq(a, b).map(v => ValueUse(Hardware.toSignal(v).valueId))))

    Hardware.fromSignal(res)

given [W <: Int : KnownInt](using Context): Add[UInt[W]] = given_Add_GenericBits
given [W <: Int : KnownInt](using Context): Add[SInt[W]] = given_Add_GenericBits
given [W <: Int : KnownInt](using Context): Add[BitVec[W]] = given_Add_GenericBits
given [W <: Int : KnownInt](using Context): Sub[UInt[W]] = given_Sub_GenericBits
given [W <: Int : KnownInt](using Context): Sub[SInt[W]] = given_Sub_GenericBits
given [W <: Int : KnownInt](using Context): Sub[BitVec[W]] = given_Sub_GenericBits

object GenericBits:
  def literal[W <: Int : KnownInt, Sg <: Type.Signedness : KnownEnum]
    (value: Int)(using cx: Context): GenericBits[W, Sg] =
    import scirt.mlir.*
    import Type.*
    import Attribute.*

    val ty: Integer = Integer(KnownInt[W], KnownEnum[Sg])
    val res = cx.allocate("lit")

    cx.add(Operation(
      OperationId("hw.constant"),
      Function(Seq(), Seq(ty)),
      results = Seq(res.valueId).map(OpResult(_)),
      attrs = Dictionary(Seq("value" -> IntegerAttr(value, ty)))))

    Hardware.fromSignal(res)

object UInt:
  extension (value: Int)
    def UI[W <: Int : KnownInt](using cx: Context): UInt[W] =
      GenericBits.literal(value)

object SInt:
  extension (value: Int)
    def SI[W <: Int : KnownInt](using cx: Context): SInt[W] =
      GenericBits.literal(value)

object BitVec:
  extension (value: Int)
    def I[W <: Int : KnownInt](using cx: Context): BitVec[W] =
      GenericBits.literal(value)
