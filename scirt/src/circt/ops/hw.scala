package scirt.circt.ops

import scirt.mlir.*

object hw:
  object module:
    object extern:
      def apply(
        name: String,
        inputs: Seq[(String, Type)], outputs: Seq[(String, Type)],
      ): Operation =
        import Attribute.*
        import Type.*

        Operation(
          OperationId("hw.module.extern"),
          Function(Seq(), Seq()),
          regions = Seq(Region(Seq())),
          attrs = Dictionary(Seq(
            "argNames" -> Array(inputs.map(_._1).map(StringAttr(_))),
            "resultNames" -> Array(outputs.map(_._1).map(StringAttr(_))),
            "sym_name" -> StringAttr(name),
            "type" -> TypeAttr(Function(inputs.map(_._2), outputs.map(_._2))),
            "parameters" -> Array(Seq()),
            "comment" -> StringAttr(""))))

    def apply(
      name: String,
      inputs: Seq[(String, ValueId, Type)], outputs: Seq[(String, Type)],
      ops: Seq[Operation]
    ): Operation =
      import Attribute.*
      import Type.*

      val block = inputs match
        case Seq() => Region(ops)
        case _ => Region(
          entry = Seq(),
          blocks = Seq(
            Block(
              BlockId("entry"),
              inputs.map((name, vid, ty) => BlockArg(vid, ty)),
              ops = ops)))

      Operation(
        OperationId("hw.module"),
        Function(Seq(), Seq()),
        regions = Seq(block),
        attrs = Dictionary(Seq(
          "argNames" -> Array(inputs.map(_._1).map(StringAttr(_))),
          "resultNames" -> Array(outputs.map(_._1).map(StringAttr(_))),
          "sym_name" -> StringAttr(name),
          "type" -> TypeAttr(Function(inputs.map(_._3), outputs.map(_._2))),
          "parameters" -> Array(Seq()),
          "comment" -> StringAttr(""))))

  object instance:
    def apply(
      name: String,
      ref: SymbolRefId,
      inputs: Seq[(String, ValueId, Type)], outputs: Seq[(String, ValueId, Type)],
    ): Operation =
      import Attribute.*
      import Type.*

      Operation(
        OperationId("hw.instance"),
        Function(inputs.map(_._3), outputs.map(_._3)),
        results = outputs.map(_._2).map(OpResult(_)),
        uses = inputs.map(_._2).map(ValueUse(_)),
        attrs = Dictionary(Seq(
          "argNames" -> Array(inputs.map(_._1).map(StringAttr(_))),
          "resultNames" -> Array(outputs.map(_._1).map(StringAttr(_))),
          "instanceName" -> StringAttr(name),
          "moduleName" -> SymbolRef(Seq(ref)),
          "parameters" -> Array(Seq()))))

  object output:
    def apply(outputs: Seq[(ValueId, Type)]): Operation =
      import Type.*

      Operation(
        OperationId("hw.output"),
        Function(outputs.map(_._2), Seq()),
        uses = outputs.map(_._1).map(ValueUse(_)))
