package scirt.mlir

type IntLiteral = Int

def validSuffixId(id: String): Boolean =
  id matches "[0-9]+|[a-zA-Z$._-][0-9a-zA-Z$._-]*"

def validBareId(id: String): Boolean =
  id matches raw"[a-zA-Z_][0-9a-zA-Z_$$.]*"

def serializeString(str: String): String =
  val escape: Char => String =
    case '"' => "\\\""
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\u000b' => "\\v"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case x => x.toString

  s"\"${str.flatMap(escape)}\""

def indented(lines: Seq[String]): Seq[String] =
  lines.map("  " ++ _)

case class ValueId(id: String):
  if ! validSuffixId(id) then
    throw RuntimeException("Invalid format for identifier")

  // `%` suffix-id
  def pretty: String = "%" + id

case class BlockId(id: String):
  if ! validSuffixId(id) then
    throw RuntimeException("Invalid format for identifier")

  // `^` suffix-id
  def pretty: String = "^" + id

case class OperationId(id: String):
  // string-literal
  def pretty: String = serializeString(id)

case class DialectId(id: String):
  if ! validBareId(id) then
    throw RuntimeException("Invalid format for identifier")

  // bare-id
  def pretty: String = id

case class TypeAliasId(id: String):
  if ! validBareId(id) then
    throw RuntimeException("Invalid format for identifier")

  // `!` bare-id
  def pretty: String = "!" + id

case class AttributeAliasId(id: String):
  if ! validBareId(id) then
    throw RuntimeException("Invalid format for identifier")

  // `#` bare-id
  def pretty: String = "!" + id


case class SymbolRefId(id: String):
  def pretty: String = "@" + serializeString(id)

enum Toplevel:
  case OperationTop(operation: Operation)
  case AttributeAlias(id: AttributeAliasId, value: Attribute)
  case TypeAlias(id: TypeAliasId, ty: Type)

  def prettyBlock: Seq[String] = this match
    case OperationTop(operation) =>
      operation.prettyBlock
    case AttributeAlias(id, value) =>
      Seq(s"${id.pretty} = ${value.pretty}")
    case TypeAlias(id, ty) =>
      Seq(s"${id.pretty} = type ${ty.pretty}")

  def pretty: String = prettyBlock.map(_ + "\n").mkString("")

case class Operation(
  op: OperationId,
  functionType: Type.Function,
  results: Seq[OpResult] = Seq(),
  uses: Seq[ValueUse] = Seq(),
  successors: Seq[BlockId] = Seq(),
  regions: Seq[Region] = Seq(),
  attrs: Attribute.Dictionary = Attribute.Dictionary(Seq()),
  loc: Location = Location.Unknown):

  def prettyBlock: Seq[String] =
    val resultsPart = results match
      case Seq() => ""
      case _ => s"${results.map(_.pretty).mkString(", ")} = "

    val successorsPart = successors match
      case Seq() => ""
      case _ => s" [${successors.map(_.pretty).mkString(", ")}]"

    val headerPart =
        s"${resultsPart}" +
          s"${op.pretty}(${uses.map(_.pretty).mkString(", ")})" +
          successorsPart
    val trailingPart =
      s"${attrs.pretty} : ${functionType.pretty} loc(${loc.pretty})"

    regions match
      case Seq() => Seq(s"${headerPart} ${trailingPart}")
      case _ =>
        s"${headerPart} ({"
          +: regions.map(_.prettyBlockInside).reduce(_ ++ Seq("}, {") ++ _)
          :+ s"}) ${trailingPart}"

object Operation:
  def simple(op: OperationId, inputs: Seq[(ValueId, Type)], outputs: Seq[(ValueId, Type)]): Operation =
    Operation(
      op,
      Type.Function(inputs.map(_._2), outputs.map(_._2)),
      results = outputs.map(_._1).map(OpResult(_)),
      uses = inputs.map(_._1).map(ValueUse(_)))

case class Region(entry: Seq[Operation], blocks: Seq[Block] = Seq()):
  def prettyBlockInside: Seq[String] =
    indented(entry.flatMap(_.prettyBlock))
    ++ blocks.flatMap(_.prettyBlock)

case class Block(id: BlockId, args: Seq[BlockArg], ops: Seq[Operation]):
  def prettyBlock: Seq[String] =
    s"${id.pretty}(${args.map(_.pretty).mkString(", ")}):"
      +: indented(ops.flatMap(_.prettyBlock))

case class OpResult(id: ValueId, count: Option[IntLiteral] = None):
  if ! count.fold(true)(_ > 0) then
    throw RuntimeException("Result count must be positive")

  def pretty: String = count match
    case Some(n) => s"${id.pretty}:${n}"
    case None => id.pretty

case class ValueUse(id: ValueId, index: Option[IntLiteral] = None):
  def pretty: String = index match
    case Some(n) => s"${id.pretty}#${n}"
    case None => id.pretty

case class BlockArg(id: ValueId, valueType: Type):
  def pretty: String =
    s"${id.pretty}: ${valueType.pretty}"


enum Type:
  case Integer(width: IntLiteral, signedness: Type.Signedness)
  case Function(params: Seq[Type], results: Seq[Type])
  case Dialect(dialect: DialectId, data: String)
  case Alias(id: TypeAliasId)

  def pretty: String = this match
    case Integer(width, signedness) =>
      import Type.Signedness._

      val prefix = signedness match
        case Signed => "si"
        case Unsigned => "ui"
        case Signless => "i"

      if width <= 0 then
        throw RuntimeException("Integer width must be positive")

      s"${prefix}${width}"

    case Function(params, results) =>
      s"(${params.map(_.pretty).mkString(", ")}) -> (${results.map(_.pretty).mkString(", ")})"

    case Dialect(dialect, data) => s"!${dialect.pretty}<${serializeString(data)}>"
    case Alias(id) => id.pretty
  end pretty

object Type:
  enum Signedness:
    case Signed, Unsigned, Signless


enum Attribute:
  case Array(elems: Seq[Attribute])
  case Dictionary(elems: Seq[(String, Attribute)])
  case TypeAttr(ty: Type)
  case StringAttr(string: String)
  case Dialect(dialect: DialectId, data: String)
  case SymbolRef(sym: Seq[SymbolRefId])
  case Alias(id: AttributeAliasId)

  def pretty: String = this match
    case Array(elems) =>
      s"[${elems.map(_.pretty).mkString(", ")}]"

    case Dictionary(elems) =>
      s"{${elems.map(
        (name, attr)=> s"${serializeString(name)} = ${attr.pretty}"
      ).mkString(", ")}}"

    case Dialect(dialect, data) =>
      s"#${dialect.pretty}<${serializeString(data)}>"

    case TypeAttr(ty) => ty.pretty
    case StringAttr(string) => serializeString(string)

    case SymbolRef(sym) =>
      if sym.isEmpty then throw RuntimeException("Symbol cannot be empty")
      sym.map(_.pretty).mkString("::")

    case Alias(id) => id.pretty

  end pretty

enum Location:
  case CallSite(callee: Location, caller: Location)
  case FileLineCol(file: String, line: IntLiteral, col: IntLiteral)
  case Fused(metadata: Option[Attribute], locs: Seq[Location])
  case Name(name: String, attached: Option[Location])
  case Unknown

  def pretty: String = this match
    case CallSite(callee, caller) =>
      s"callsite(${callee.pretty} at ${caller.pretty})"
    case FileLineCol(file, line, col) =>
      s"${serializeString(file)}:${line}:${col}"
    case Fused(metadata, locs) =>
      s"fused${metadata.fold("")(a => s"<${a.pretty}>")}[${locs.map(_.pretty).mkString(", ")}]"
    case Name(name, attached) =>
      s"${serializeString(name)}${attached.fold("")(loc => s"(${loc.pretty})")}"
    case Unknown =>
      "unknown"
