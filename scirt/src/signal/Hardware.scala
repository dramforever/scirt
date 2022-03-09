package scirt.signal

import scirt.mlir.*

trait Hardware[T]:
  def underlyingType: Type
  def toSignal(value: T): Signal
  def fromSignal(signal: Signal): T

object Hardware:
  def underlyingType[T : Hardware]: Type =
    summon[Hardware[T]].underlyingType

  def toSignal[T : Hardware](value: T): Signal =
    summon[Hardware[T]].toSignal(value)

  def fromSignal[T : Hardware](signal: Signal) =
    summon[Hardware[T]].fromSignal(signal)
