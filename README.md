# rand

Random data generator for scala.

## Usage

Example usage:

```scala
import astrac.rand.AutoRand
import cats.syntax.all.given

enum Animal derives AutoRand:
  case Dog(name: String, isGood: Boolean)
  case Cat(name: String, livesSpent: Long)

sealed trait Shape derives AutoRand
case class Circle(radius: Long, label: Option[String]) extends Shape
case class Square(side: Long, label: Option[String]) extends Shape

// Combines two auto-derived rands using the Monad instance
val (animal, shape) =
  val rand = for
    animal <- AutoRand[Animal].rand
    shape <- AutoRand[Shape].rand
  yield (animal, shape)
  rand.unsafeRun()
```

TODO:

* Implement auto-derivation for `AutoCoRand` (needed for derivation of AutoRand for functions)
* Customizable primitive `AutoRand` instances for types such as `String` or `Long`
* Integration with test frameworks
