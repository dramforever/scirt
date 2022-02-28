package scirt.dsl.types

import scirt.mlir.*
import scirt.signal.*

import Type.Signedness.*
import scirt.utils.{given, *}

// FIXME: When we eventually write higher-level modules, re-export these at the
// user-level modules instead.
export scirt.utils.knownIntInstances.given
export scirt.utils.knownEnumMacro

class GenericBits[W <: Int, Sg <: Type.Signedness](val signal: Signal)

opaque type UInt[W <: Int] = GenericBits[W, Unsigned.type]
opaque type SInt[W <: Int] = GenericBits[W, Signed.type]
opaque type BitVec[W <: Int] = GenericBits[W, Signless.type]

given given_Hardware_Bits[W <: Int : KnownInt, Sg <: Type.Signedness : KnownEnum]: Hardware[GenericBits[W, Sg]] with
  def underlyingType = Type.Integer(KnownInt[W], KnownEnum[Sg])
  def fromSignal(signal: Signal ) = GenericBits[W, Sg](signal)
  def toSignal(value: GenericBits[W, Sg]) = value.signal

given [W <: Int : KnownInt]: Hardware[UInt[W]] = given_Hardware_Bits
given [W <: Int : KnownInt]: Hardware[SInt[W]] = given_Hardware_Bits
given [W <: Int : KnownInt]: Hardware[BitVec[W]] = given_Hardware_Bits
