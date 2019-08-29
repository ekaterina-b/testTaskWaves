package models

object OperationType extends Enumeration{
  type OperationType = Value
  val sell = Value("s")
  val buy = Value("b")
  def apply(v: String): OperationType = v match {
    case "s" => sell
    case "b" => buy
  }
}
