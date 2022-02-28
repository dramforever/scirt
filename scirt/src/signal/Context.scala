package scirt.signal

import scirt.mlir.Operation

trait Context:
  def allocate(name: String = "_"): Signal
  def accessible(signal: Signal): Boolean
  def add(op: Operation): Unit
