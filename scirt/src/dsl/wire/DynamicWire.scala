package scirt.dsl.wire

import scirt.signal.*
import scirt.mlir.{ValueId, Type, Operation}
import scirt.circt.ops
import scirt.utils.passOp

class DynamicWire(val signal: Signal, val ty: Type)(using ctx: Context):
  var connected = false

  def get[T : Hardware]: T =
    assert(ty == Hardware.underlyingType[T], "DynamicWire with wrong type")
    Hardware.fromSignal[T](signal)

  def :=[T : Hardware](value: T): Unit =
    assert(ty == Hardware.underlyingType[T], "DynamicWire with wrong type")

    if connected then
      throw RuntimeException("This Wire is already connected")
    else
      connected = true

      ctx.add(
        passOp(
          signal.valueId,
          Hardware.toSignal(value).valueId,
          Hardware.underlyingType[T]))

object DynamicWire:
  def apply(ty: Type, name: String = "wire")(using ctx: Context): DynamicWire =
    new DynamicWire(ctx.allocate(name), ty)

  def fromSignal(ty: Type, signal: Signal)(using Context): DynamicWire =
    new DynamicWire(signal, ty)
