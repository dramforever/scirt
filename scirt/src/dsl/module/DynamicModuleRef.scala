package scirt.dsl.module

import scirt.mlir.Type
import scirt.signal.*
import scirt.mlir.*
import scirt.dsl.ops.*
import scirt.circt.types
import scirt.dsl.wire.DynamicWire
import scirt.circt.ops

class DynamicModuleRef(
  val symbol: String,
  val inputs: Map[String, Type],
  val outputs: Map[String, Type]):

  def instance(using cx: Context): DynamicPorts =
    val inputWires =
      inputs.map((name, ty) => name -> DynamicWire(ty, name)).toMap
    val outputSignals =
      outputs.map((name, ty) => name -> cx.allocate(name)).toMap

    cx.add(
      ops.hw.instance(
        name = cx.allocateInstanceName(),
        ref = SymbolRefId(symbol),
        inputs.map((name, ty) => (name, inputWires(name).signal.valueId, ty)).toSeq,
        outputs.map((name, ty) => (name, outputSignals(name).valueId, ty)).toSeq,
      ))

    DynamicPorts(inputWires, outputSignals)
