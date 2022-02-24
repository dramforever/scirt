package scirt.mlir

type IntLiteral = Int

case class ValueId(id: String)
case class BlockId(id: String)
case class OperationName(id: String)
case class DialectName(id: String)
case class TypeAliasId(id: String)
case class AttributeAliasId(id: String)

enum Toplevel:
  case OperationTop(operation: Operation)
  case AttributeAlias(id: AttributeAliasId, value: Attribute)
  case TypeAlias(id: TypeAliasId, ty: Type)


case class Operation(
  op: OperationName,
  functionType: Type.Function,
  results: Seq[OpResult],
  valueUses: Seq[ValueUse],
  successors: Seq[Successor] = Seq(),
  regions: Seq[Region] = Seq(),
  dictionaryAttribute: Attribute.Dictionary = Attribute.Dictionary(Seq()),
  loc: Location = Location.Unknown)


case class Region(entryBlock: Seq[Operation], blocks: Seq[Block])
case class Block(id: BlockId, args: Seq[BlockArg], ops: Seq[Operation])

case class OpResult(id: ValueId, count: Option[IntLiteral] = None)
case class ValueUse(id: ValueId, index: Option[IntLiteral] = None)
case class Successor(id: BlockId, args: Seq[BlockArg] = Seq())

case class BlockArg(id: ValueId, valueType: Type)


enum Type:
  case Integer(width: IntLiteral, signedness: Type.Signedness)
  case Function(params: Seq[Type], results: Seq[Type])
  case Dialect(dialect: DialectName, data: String)
  case Alias(id: TypeAliasId)

object Type:
  enum Signedness:
    case Signed, Unsigned, Signless


enum Attribute:
  case Array(elems: Seq[Attribute])
  case Dictionary(elems: Seq[(String, Attribute)])
  case TypeAttr(ty: Type)
  case StringAttr(string: String)
  case Alias(id: AttributeAliasId)


enum Location:
  case CallSite(callee: Location, caller: Location)
  case FileLineCol(file: String, line: IntLiteral, col: IntLiteral)
  case Fused(metadata: Attribute, locs: Seq[Location])
  case Name(name: String, attached: Option[Location])
  case Unknown
