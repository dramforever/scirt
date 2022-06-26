package scirt.dsl.types

import scirt.signal.Signal
import scirt.dsl.ops.*
import scirt.signal.Hardware
import scirt.mlir.Type
import scirt.circt.types

class Reset(val signal: Signal)

given given_Hardware_Reset: Hardware[Reset] with
  def underlyingType: Type = types.builtin.i(1)
  def fromSignal(signal: Signal): Reset = Reset(signal)
  def toSignal(clock: Reset): Signal = clock.signal
