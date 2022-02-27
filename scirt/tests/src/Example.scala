package scirt.tests.circt

import scirt.circt.*
import scirt.mlir.*
import utest.*

// TODO: add circt-opt CI for validation
object Example extends TestSuite:
  val tests = Tests {
    test("this is a test") {
      Toplevel
        .OperationTop(
          ops.builtin.module(
            name = Some("design"),
            ops = Seq(
              ops.hw.moduleExtern("uwu", Seq("d" -> types.builtin.i(32)), Seq("q" -> types.builtin.i(32))),
              ops.hw.module(
                "add",
                inputs = Seq(("a", ValueId("0"), types.builtin.i(32))),
                outputs = Seq(("o", types.builtin.i(32))),
                ops = Seq(
                  ops.hw.instance("inst", SymbolRefId("uwu"), Seq(("d", ValueId("0"), types.builtin.i(32))), Seq(("q", ValueId("1"), types.builtin.i(32)))),
                  Operation.simple(
                    OperationId("comb.add"),
                    Seq(ValueId("1") -> types.builtin.i(32), ValueId("2") -> types.builtin.i(32)),
                    Seq(ValueId("2") -> types.builtin.i(32))
                  ),
                  ops.hw.output(Seq((ValueId("2"), types.builtin.i(32))))
                )
              )
            )
          )
        )
        .pretty
    }
  }