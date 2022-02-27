package scirt.circt.ops

import scirt.mlir._

object builtin:
  object module
    def apply(ops: Seq[Operation], name: Option[String] = None): Operation =
      import Attribute._
      import Type._

      Operation(
        OperationId("builtin.module"),
        Function(Seq(), Seq()),
        regions = Seq(Region(entry = ops)),
        attrs = Dictionary(name.map(
          name => "sym_name" -> StringAttr(name)).toSeq))
