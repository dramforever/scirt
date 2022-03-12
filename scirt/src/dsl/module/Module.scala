package scirt.dsl.module

import scirt.signal.*
import scirt.mlir.{Type, Operation}
import scirt.circt.ops

import scala.collection.mutable

class Module extends Context.Basic:
  val inputs = mutable.Buffer[(String, Signal, Type)]()
  val outputs = mutable.Buffer[(String, Signal, Type)]()

  def allocateInput(name: String, ty: Type): Signal =
    val sig = allocate(name)
    inputs += ((name, sig, ty))
    sig

  def recordOutput(name: String, sig: Signal, ty: Type): Unit =
    outputs += ((name, sig, ty))

  def build(name: String): Operation =
    ops.hw.module(
      name,
      inputs.toSeq.map((name, sig, ty) => (name, sig.valueId, ty)),
      outputs.toSeq.map((name, sig, ty) => (name, ty)),
      currentOps.toSeq
        :+ ops.hw.output(outputs.toSeq.map((_, sig, ty) => (sig.valueId, ty))))

object Module:
  def apply(name: String)(body: Module ?=> Unit): Operation =
    val mod = new Module()
    body(using mod)
    mod.build(name)

def input[T : Hardware](name: String)(using mod: Module): T =
  Hardware.fromSignal(mod.allocateInput(name, Hardware.underlyingType[T]))

def output[T : Hardware](name: String, value: T)(using mod: Module): Unit =
  mod.recordOutput(name, Hardware.toSignal(value), Hardware.underlyingType[T])