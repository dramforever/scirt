package scirt.dsl.module

import scirt.signal.*
import scirt.mlir.{Type, Operation}
import scirt.circt.ops

import scala.collection.mutable

class Module[P <: Ports : Ports.Known] extends Context.Basic:
  val inputs: Map[String, (Signal, Type)] =
    summon[Ports.Known[P]].has.map((name, ty) => (name, (allocate(name), ty)))

  def outputTypes: Map[String, Type] = summon[Ports.Known[P]].needs
  val outputs: mutable.Buffer[(String, Signal, Type)] = mutable.Buffer()

  def getInput(name: String): Signal =
    inputs(name)._1

  def setOutput(name: String, value: Signal): Unit =
    outputs += ((name, value, outputTypes(name)))

  def build(name: String): Operation =
    ops.hw.module(
      name,
      inputs.toSeq.map { case (name, (sig, ty)) => (name, sig.valueId, ty) },
      outputs.toSeq.map((name, sig, ty) => (name, ty)),
      currentOps.toSeq
        :+ ops.hw.output(outputs.toSeq.map((_, sig, ty) => (sig.valueId, ty))))

def io[P <: Ports](using Module[P]): P = Ports().asInstanceOf

object Module:
  def apply[P <: Ports : Ports.Known](name: String)(body: Module[P] ?=> Unit): Operation =
    val mod = new Module[P]()
    body(using mod)
    mod.build(name)
