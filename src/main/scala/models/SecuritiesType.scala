package models

object SecuritiesType extends Enumeration{
  type SecuritiesType = Value
  val securitiesA = Value("A")
  val securitiesB = Value("B")
  val securitiesC = Value("C")
  val securitiesD = Value("D")

  def apply(v: String): SecuritiesType = v match {
    case "A"    => securitiesA
    case "B" => securitiesB
    case "C" => securitiesC
    case "D" => securitiesD
    case _   => throw new IllegalArgumentException()
  }
}

