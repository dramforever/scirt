package scirt.utils

import scirt.mlir.{ValueId, Type, Operation}

// FIXME: Using bitcast really is a hack. Also, CIRCT upstream already moved
// hw.bitcast to comb.bitcast.
def passOp(output: ValueId, input: ValueId, ty: Type): Operation =
  import scirt.mlir.*

  Operation(
    OperationId("hw.bitcast"),
    Type.Function(Seq(ty), Seq(ty)),
    Seq(OpResult(output)),
    Seq(ValueUse(input)))
