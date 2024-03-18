import astrac.rand.AutoRand
import cats.syntax.all.given

enum Animal derives AutoRand:
  case Dog(name: String, isGood: Boolean)
  case Cat(name: String, livesSpent: Long)

sealed trait Shape derives AutoRand
case class Circle(radius: Long, label: Option[String]) extends Shape
case class Square(side: Long, label: Option[String]) extends Shape

val (animal, shape) =
  val rand = for
    animal <- AutoRand[Animal].rand
    shape <- AutoRand[Shape].rand
  yield (animal, shape)
  rand.unsafeRun(123456L)
