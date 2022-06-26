package scirt.dsl.module

import scirt.dsl.module.*
import scirt.mlir.*
import scirt.signal.Context
import scirt.dsl.module.Ports
import scirt.circt.ops

class ModuleRef[P <: Ports : Ports.Known](
  val symbol: String,
  val inputs: Map[String, Type],
  val outputs: Map[String, Type]):

  def instance(using cx: Context, flipped: Ports.Flipped[P]): flipped.T =
    val inputSignals =
      inputs.map((name, ty) => name -> cx.allocate(name)).toMap
    val outputSignals =
      outputs.map((name, ty) => name -> cx.allocate(name)).toMap

    cx.add(
      ops.hw.instance(
        name = cx.allocateInstanceName(),
        ref = SymbolRefId(symbol),
        inputs.map((name, ty) => (name, inputSignals(name).valueId, ty)).toSeq,
        outputs.map((name, ty) => (name, outputSignals(name).valueId, ty)).toSeq,
      ))

    Ports(ModuleInstance(outputSignals, inputSignals)).asInstanceOf[flipped.T]
