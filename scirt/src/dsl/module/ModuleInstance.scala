package scirt.dsl.module

import scirt.signal.Signal

class ModuleInstance(
  val inputs: Map[String, Signal],
  val outputs: Map[String, Signal]
) extends HasPorts:
  def getInput(name: String): Signal = inputs(name)
  def getOutput(name: String): Signal = outputs(name)
