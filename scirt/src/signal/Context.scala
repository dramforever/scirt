package scirt.signal

import scirt.mlir.Operation

import scala.collection.mutable
import scirt.mlir.ValueId

trait Context:
  def allocate(name: String = "_"): Signal
  def accessible(signal: Signal): Boolean
  def add(op: Operation): Unit

object Context:
  class Basic extends Context:
    val usedNames = mutable.Set[String]()
    val ownSignals = mutable.Set[Signal]()
    var counter = 0

    val ops = mutable.Buffer[Operation]()

    def allocate(name: String) : Signal =
      while usedNames.contains(s"${name}${counter}") do
        counter += 1

      val uniqueName = s"${name}${counter}"
      usedNames += uniqueName

      val signal = Signal(ValueId(uniqueName))
      ownSignals += signal

      signal

    def accessible(signal: Signal) = ownSignals.contains(signal)
    def add(op: Operation) = ops += op
