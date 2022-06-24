package scirt.dsl.design

import scala.collection.mutable
import scirt.mlir.*

class Design:
  val modules = mutable.Buffer[Operation]()

  def +=(op: Operation): Unit = modules += op

object Design:
  def apply(body: Design ?=> Unit): Seq[Operation] =
    val design = new Design()
    body(using design)
    design.modules.toSeq
