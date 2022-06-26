package scirt.dsl.wire

import scirt.signal.*
import scirt.circt.ops
import scirt.utils.passOp

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
