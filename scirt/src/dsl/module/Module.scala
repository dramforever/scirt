package scirt.dsl.module

import scirt.signal.*
import scirt.mlir.{Type, Operation}
import scirt.circt.ops

import scala.collection.mutable

class Module[P <: Ports : Ports.Known] extends Context.Basic with HasPorts:
  val ports: P = Ports(this).asInstanceOf

  val inputs: Map[String, (Signal, Type)] =
    summon[Ports.Known[P]].has.map((name, ty) => (name, (allocate(name), ty)))

  def outputTypes: Map[String, Type] = summon[Ports.Known[P]].needs
  val outputs: Map[String, (Signal, Type)] =
    summon[Ports.Known[P]].needs.map((name, ty) => (name, (allocate(name), ty)))

  def getInput(name: String): Signal = inputs(name)._1
  def getOutput(name: String): Signal = outputs(name)._1

  def build(name: String): Operation =
    ops.hw.module(
      name,
      inputs.toSeq.map { case (name, (sig, ty)) => (name, sig.valueId, ty) },
      outputs.toSeq.map { case (name, (sig, ty)) => (name, ty) },
      currentOps.toSeq
        :+ ops.hw.output(outputs.values.toSeq.map((sig, ty) => (sig.valueId, ty))))

transparent inline def io[P <: Ports](using inline q: Module[P]): P = q.ports

object Module:
  def apply[P <: Ports : Ports.Known](name: String)(body: Module[P] ?=> Unit): Operation =
    val mod = new Module[P]()
    body(using mod)
    mod.build(name)
