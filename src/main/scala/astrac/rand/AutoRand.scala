package astrac.rand

import astrac.rand.derivation.DerivedAutoRand
import scala.deriving.Mirror
import cats.Monad
import cats.syntax.applicative.given
import cats.syntax.flatMap.given
import scala.util.TupledFunction
import scala.annotation.experimental

class AutoRand[A](val rand: Rand[A]) extends AnyVal

object AutoRand:
  def apply[A: AutoRand]: AutoRand[A] = summon
  def instance[A](p: Rand[A]): AutoRand[A] = new AutoRand(p)

  given Monad[AutoRand] with
    def pure[A](x: A): AutoRand[A] = AutoRand.instance(x.pure)
    def flatMap[A, B](fa: AutoRand[A])(f: A => AutoRand[B]): AutoRand[B] =
      AutoRand.instance(fa.rand.flatMap(f.andThen(_.rand)))
    def tailRecM[A, B](a: A)(f: A => AutoRand[Either[A, B]]): AutoRand[B] =
      AutoRand.instance(a.tailRecM(f.andThen(_.rand)))

  given AutoRand[Boolean] = AutoRand.instance(Rand.boolean)
  given AutoRand[Char] = AutoRand.instance(Rand.char)
  given AutoRand[Double] = AutoRand.instance(Rand.double)
  given AutoRand[Float] = AutoRand.instance(Rand.float)
  given AutoRand[Int] = AutoRand.instance(Rand.int)
  given [A: AutoRand]: AutoRand[Option[A]] =
    AutoRand.instance(Rand.option(AutoRand[A].rand))
  given AutoRand[Long] = AutoRand.instance(Rand.long)
  given AutoRand[String] = AutoRand.instance(Rand.string)

  @experimental
  given [F, Args <: Tuple, R](using
      TupledFunction[F, Args => R],
      AutoCoRand[Args],
      AutoRand[R]
  ): AutoRand[F] =
    AutoRand.instance(Rand.function(AutoCoRand[Args].coRand, AutoRand[R].rand))

  inline given [T <: Tuple: Mirror.ProductOf]: AutoRand[T] = DerivedAutoRand[T]

  inline def derived[A](using m: Mirror.Of[A]): AutoRand[A] = DerivedAutoRand[A]
