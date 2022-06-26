package scirt.dsl.module

import scirt.signal.Signal

trait HasPorts:
  def getInput(name: String): Signal
  def getOutput(name: String): Signal

