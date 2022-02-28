package scirt.signal

import scirt.mlir.ValueId

/**
 * A thin wrapper around `Signal` with equality defined as reference equality. A
 * DSL `Context` can keep track of what values are in scope with this type
 * rather than by name only.
 */
class Signal(val valueId: ValueId):
  override def equals(other: Any): Boolean = other match
    case other: AnyRef => this eq other
    case _ => false

  override def hashCode: Int = System.identityHashCode(this)
