package scirt.dsl.ops

trait Add[T]:
  def add(a: T, b: T): T
  extension (a: T) def +(b: T) = add(a, b)

trait Sub[T]:
  def sub(a: T, b: T): T
  extension (a: T) def -(b: T) = sub(a, b)
