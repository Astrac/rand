package astrac.rand

import astrac.rand.derivation.DerivedCoRand
import scala.deriving.Mirror

class AutoCoRand[A](val coRand: CoRand[A]) extends AnyVal

object AutoCoRand:
  def apply[A: AutoCoRand]: AutoCoRand[A] = summon
  def instance[A](p: CoRand[A]): AutoCoRand[A] = new AutoCoRand(p)

  given AutoCoRand[Boolean] = AutoCoRand.instance(CoRand.boolean)
  given AutoCoRand[Char] = AutoCoRand.instance(CoRand.char)
  given AutoCoRand[Double] = AutoCoRand.instance(CoRand.double)
  given AutoCoRand[Float] = AutoCoRand.instance(CoRand.float)
  given AutoCoRand[Int] = AutoCoRand.instance(CoRand.int)
  given AutoCoRand[Long] = AutoCoRand.instance(CoRand.long)
  given AutoCoRand[String] = AutoCoRand.instance(CoRand.string)

  inline given AutoCoRand[EmptyTuple] =
    AutoCoRand.instance(CoRand(_ => Rand.skip))

  inline given [T <: NonEmptyTuple: Mirror.Of]: AutoCoRand[T] = DerivedCoRand[T]
