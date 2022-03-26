package scirt.dsl.wire

import scirt.signal.*
import scirt.mlir.{ValueId, Type, Operation}
import scirt.circt.ops

// FIXME: Using bitcast really is a hack. Also, CIRCT upstream already moved
// hw.bitcast to comb.bitcast.
private def passOp(output: ValueId, input: ValueId, ty: Type): Operation =
  import scirt.mlir.*

  Operation(
    OperationId("hw.bitcast"),
    Type.Function(Seq(ty), Seq(ty)),
    Seq(OpResult(output)),
    Seq(ValueUse(input)))

class Wire[T : Hardware](val signal: Signal)(using ctx: Context):
  var connected = false

  def get: T =
    Hardware.fromSignal[T](signal)

  def :=(value: T): Unit =
    if connected then
      throw RuntimeException("This Wire is already connected")
    else
      connected = true

      ctx.add(
        passOp(
          signal.valueId,
          Hardware.toSignal(value).valueId,
          Hardware.underlyingType[T]))

object Wire:
  def apply[T : Hardware](name: String = "wire")(using ctx: Context): Wire[T] =
    new Wire(ctx.allocate(name))

  def fromSignal[T : Hardware](signal: Signal)(using Context): Wire[T] =
    new Wire(signal)

type Output[T] = Wire[T]
