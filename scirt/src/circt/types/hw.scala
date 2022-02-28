package scirt.circt.types

import scirt.mlir.*

object hw:
  def apply(data: String): Type = Type.Dialect(DialectId("hw"), data)

  def array(width: Int, element: Type): Type =
    if width < 0 then throw RuntimeException("hw.array width must be non-negative")

    hw(s"array<${width} x ${element.pretty}>")

  def struct(fields: Seq[(String, Type)]): Type =
    def genField(name: String, ty: Type): String =
      if ! validBareId(name) then throw RuntimeException("Invalid struct field name")
      s"${name}: ${ty.pretty}"

    hw(s"struct<${fields.map(genField).mkString(", ")}>")
