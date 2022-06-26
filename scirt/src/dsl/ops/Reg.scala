package scirt.dsl.ops

import scirt.signal.Hardware
import scirt.signal.Context
import scirt.signal.Signal
import scirt.mlir.*
import scirt.utils.passOp
import scirt.dsl.types.Clock
import scirt.circt.types

class Reg[T : Hardware](d: Signal, q: Signal):
  def get: T = Hardware.fromSignal[T](q)

  def :=(value: T)(using ctx: Context): Unit =
    ctx.add(
      passOp(
        d.valueId,
        Hardware.toSignal(value).valueId,
        Hardware.underlyingType[T]))

object Reg:
  def apply[T : Hardware](clock: Clock)(using ctx: Context): Reg[T] =
    val q = ctx.allocate()
    val d = ctx.allocate()

    val ty = Hardware.underlyingType[T]

    ctx.add(
      Operation(
        OperationId("seq.compreg"),
        Type.Function(
          Seq(ty, types.builtin.i(1)),
          Seq(ty)),
        Seq(OpResult(q.valueId)),
        Seq(ValueUse(d.valueId), ValueUse(clock.signal.valueId)),
        attrs = Attribute.Dictionary(Seq("name" -> Attribute.StringAttr("")))))

    new Reg[T](d, q)
