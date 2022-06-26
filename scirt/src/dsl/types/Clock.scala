package scirt.dsl.types

import scirt.signal.Signal
import scirt.dsl.ops.*
import scirt.signal.Hardware
import scirt.mlir.Type
import scirt.circt.types

class Clock(val signal: Signal)

given given_Hardware_Clock: Hardware[Clock] with
  def underlyingType: Type = types.builtin.i(1)
  def fromSignal(signal: Signal): Clock = Clock(signal)
  def toSignal(clock: Clock): Signal = clock.signal
