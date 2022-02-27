package scirt.circt

import scirt.mlir._
import scirt.circt._

val example: Operation =
  ops.hw.module("add",
    inputs = Seq(("a", ValueId("0"), types.builtin.i(32))),
    outputs = Seq(("o", types.builtin.i(32))),
    ops = Seq(
      ops.hw.output(Seq((ValueId("0"), types.builtin.i(32))))
    ))

@main def main =
  println(Toplevel.OperationTop(example).pretty)
