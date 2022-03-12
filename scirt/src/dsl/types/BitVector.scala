package scirt.dsl.types

import scirt.mlir.Type
import scirt.signal.*
import scirt.dsl.ops.*
import scirt.circt.types

import scirt.utils.{given, *}

// FIXME: When we eventually write higher-level modules, re-export these at the
// user-level modules instead.
export scirt.utils.knownIntInstances.given
export scirt.utils.knownEnumMacro

class BitVector[W <: Int](val signal: Signal)

given given_Hardware_BitVector[W <: Int : KnownInt]: Hardware[BitVector[W]] with
  def underlyingType = types.builtin.i(KnownInt[W])
  def fromSignal(signal: Signal ) = BitVector[W](signal)
  def toSignal(value: BitVector[W]) = value.signal

given given_Add_GenericBits[W <: Int : KnownInt](using cx: Context): Add[BitVector[W]] with
  def add(a: BitVector[W], b: BitVector[W]): BitVector[W] =
    import scirt.mlir.*
    import Type.*

    val ty = Hardware.underlyingType[BitVector[W]]
    val res = cx.allocate("add")

    cx.add(Operation(
      OperationId("comb.add"),
      Function(Seq(ty, ty), Seq(ty)),
      results = Seq(res.valueId).map(OpResult(_)),
      uses = Seq(a, b).map(v => ValueUse(Hardware.toSignal(v).valueId))))

    Hardware.fromSignal(res)

given given_Sub_GenericBits[W <: Int : KnownInt](using cx: Context): Sub[BitVector[W]] with
  def sub(a: BitVector[W], b: BitVector[W]): BitVector[W] =
    import scirt.mlir.*
    import Type.*

    val ty = Hardware.underlyingType[BitVector[W]]
    val res = cx.allocate("sub")

    cx.add(Operation(
      OperationId("comb.sub"),
      Function(Seq(ty, ty), Seq(ty)),
      results = Seq(res.valueId).map(OpResult(_)),
      uses = Seq(a, b).map(v => ValueUse(Hardware.toSignal(v).valueId))))

    Hardware.fromSignal(res)

object BitVector:
  def literal[W <: Int : KnownInt](value: Int)(using cx: Context): BitVector[W] =
    import scirt.mlir.*
    import Type.*
    import Attribute.*

    val ty: Integer = Integer(KnownInt[W], Signedness.Signless)
    val res = cx.allocate("lit")

    cx.add(Operation(
      OperationId("hw.constant"),
      Function(Seq(), Seq(ty)),
      results = Seq(res.valueId).map(OpResult(_)),
      attrs = Dictionary(Seq("value" -> IntegerAttr(value, ty)))))

    Hardware.fromSignal(res)

  extension (x: Int)
    def I[W <: Int : KnownInt](using Context): BitVector[W] = BitVector.literal[W](x)
