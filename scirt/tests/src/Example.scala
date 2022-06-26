package scirt.tests.circt

import scirt.circt.*
import scirt.mlir.*
import utest.*
import scirt.signal.Context
import scirt.dsl.types.{*, given}
import scirt.signal.Hardware
import scirt.dsl.module.*
import scirt.dsl.design.Design
import scirt.dsl.wire.*
import scirt.dsl.ops.*

// TODO: add circt-opt CI for validation
object Example extends TestSuite:
  val tests = Tests {
    test("this is a test") {
      Toplevel
        .OperationTop(
          ops.builtin.module(
            name = Some("design"),
            ops = Seq(
              ops.hw.module.extern("uwu", Seq("d" -> types.builtin.i(32)), Seq("q" -> types.builtin.i(32))),
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

object BasicContextExample extends TestSuite:
  val tests = Tests {
    test("basic context test") {
      import BitVector.I

      val cx = Context.Basic()
      val a = Hardware.fromSignal[BitVector[32]](cx.allocate("a"))
      val b = Hardware.fromSignal[BitVector[32]](cx.allocate("b"))

      val body: Context ?=> BitVector[32] =
        a + b + 3.I

      body(using cx)
    }
  }

object BuilderExample extends TestSuite:
  val tests = Tests {
    test("builder test") {

      val design = Design {
        val adder32 = DynamicModule("add32") {
          val a: BitVector[32] = input("a")
          val b: BitVector[32] = input("b")
          output[BitVector[32]]("sum") := a + b
        }

        val addmore = DynamicModule("addmore") {
          val a: BitVector[32] = input("a")
          val b: BitVector[32] = input("b")
          val c: BitVector[32] = input("c")

          val p1 = adder32.instance
          val p2 = adder32.instance

          p1.input("a") := a
          p1.input("b") := b

          p2.input("a") := p1.output[BitVector[32]]("sum")
          p2.input("b") := c

          val sum = output[BitVector[32]]("sum")

          sum := p2.output[BitVector[32]]("sum")
        }
      }

      design.foreach(_.prettyBlock.foreach(println(_)))
    }
  }

object ModuleExample extends TestSuite:
  val tests = Tests {
    test("builder test") {
      import BitVector.I

      type AdderPorts = Ports {
        val a: BitVector[32]
        val b: BitVector[32]
        val sum: Output[BitVector[32]]
      }

      type AddMorePorts = Ports {
          val a: BitVector[32]
          val b: BitVector[32]
          val c: BitVector[32]
          val sum: Output[BitVector[32]]
      }

      val design = Design {
        val adder32 = Module[AdderPorts]("static_add32") {
          import io.*

          sum := a + b
        }

        val addmore = Module[AddMorePorts]("addmore") {
          import io.*

          val p1 = adder32.instance
          val p2 = adder32.instance

          p1.a := a
          p1.b := b

          p2.a := p1.sum
          p2.b := c

          sum := p2.sum
        }

      }

      design.foreach(_.prettyBlock.foreach(println(_)))
    }
  }


object CounterExample extends TestSuite:
  val tests = Tests {
    test("builder test") {
      import BitVector.I

      type CounterPorts = Ports {
        val clk: Clock
        val delta: BitVector[32]
        val sum: Output[BitVector[32]]
      }

      val design = Design {
        val counter = Module[CounterPorts]("counter") {
          import io.*

          val ctr = Reg[BitVector[32]](clk)
          val ctr2 = Reg[BitVector[32]](clk)
          val ctr3 = Reg[BitVector[32]](clk)

          ctr := ctr.get + delta
          ctr2 := delta
          ctr3 := delta

          sum := ctr.get
        }
      }

      design.foreach(_.prettyBlock.foreach(println(_)))

    }
  }
