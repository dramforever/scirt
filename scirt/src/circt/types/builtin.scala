package scirt.circt.types

import scirt.mlir._

object builtin:
  def i(width: Int): Type.Integer = Type.Integer(width, Type.Signedness.Signless)
  def ui(width: Int): Type.Integer = Type.Integer(width, Type.Signedness.Unsigned)
  def si(width: Int): Type.Integer = Type.Integer(width, Type.Signedness.Signed)
