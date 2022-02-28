package scirt.signal

import scirt.mlir.*

trait Hardware[T]:
  def underlyingType: Type
  def toSignal(value: T): Signal
  def fromSignal(valueId: Signal): T
