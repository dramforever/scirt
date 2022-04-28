package scirt.dsl.module

import scirt.signal.*
import scirt.mlir.{Type, Operation}
import scirt.circt.ops

import scala.collection.mutable
import scirt.dsl.wire.Wire

class DynamicModule extends Context.Basic:
  val inputs = mutable.Buffer[(String, Signal, Type)]()
  val outputs = mutable.Buffer[(String, Signal, Type)]()

  def allocateInput(name: String, ty: Type): Signal =
    val sig = allocate(name)
    inputs += ((name, sig, ty))
    sig

  def allocateOutput(name: String, ty: Type): Signal =
    val sig = allocate(name)
    outputs += ((name, sig, ty))
    sig

  def build(name: String): (Operation, DynamicModuleRef) =
    val op = ops.hw.module(
      name,
      inputs.toSeq.map((name, sig, ty) => (name, sig.valueId, ty)),
      outputs.toSeq.map((name, sig, ty) => (name, ty)),
      currentOps.toSeq
        :+ ops.hw.output(outputs.toSeq.map((_, sig, ty) => (sig.valueId, ty))))

    val ref = DynamicModuleRef(
      name,
      inputs.toSeq.map((name, sig, ty) => (name, ty)).toMap,
      outputs.toSeq.map((name, sig, ty) => (name, ty)).toMap)

    (op, ref)

object DynamicModule:
  def apply(name: String)(body: DynamicModule ?=> Unit): (Operation, DynamicModuleRef) =
    val mod = new DynamicModule()
    body(using mod)
    mod.build(name)

def input[T : Hardware](name: String)(using mod: DynamicModule): T =
  Hardware.fromSignal(mod.allocateInput(name, Hardware.underlyingType[T]))

def output[T : Hardware](name: String)(using mod: DynamicModule): Wire[T] =
  Wire.fromSignal(mod.allocateOutput(name, Hardware.underlyingType[T]))
