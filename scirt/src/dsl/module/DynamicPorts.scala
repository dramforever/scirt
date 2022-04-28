package scirt.dsl.module

import scirt.signal.Signal
import scirt.dsl.wire.DynamicWire
import scirt.signal.Hardware

class DynamicPorts(
  val input: Map[String, DynamicWire],
  val outputSignals: Map[String, Signal]):

  def output[T : Hardware](name: String): T =
    // TODO: Add type check
    Hardware.fromSignal(outputSignals(name))
