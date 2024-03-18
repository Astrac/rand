package astrac.rand

import cats.syntax.arrow.given
import cats.syntax.contravariantSemigroupal.given
import cats.syntax.flatMap.given
import cats.syntax.functor.given
import cats.syntax.traverse.given
import cats.ContravariantMonoidal

// TODO: Make rand protected here
class CoRand[A](val rand: A => Rand[Unit]):
  type In = A

object CoRand:
  def instance[A](f: A => Long): CoRand[A] =
    CoRand: a =>
      val vecTrans = Rand.state.flatMap: ctx =>
        Rand.setVector:
          val n = f(a)
          val n0 = ((n >>> 32) & 0xffffffff)
          val n1 = (n & 0xffffffff)
          ctx.vector.copy(s0 = ctx.vector.s0 ^ n0, s1 = ctx.vector.s1 ^ n1)

      vecTrans >> Rand.skip

  def boolean: CoRand[Boolean] =
    CoRand.instance(b => if b then 1 else 0)

  def char: CoRand[Char] =
    CoRand.instance(identity)

  def float: CoRand[Float] =
    (long, long).contramapN[Float]: f =>
      val integerPart = math.floor(f).toLong
      val decimalPart = ((f - integerPart) * 1e18).toLong
      (integerPart, decimalPart)

  def double: CoRand[Double] =
    (long, long).contramapN[Double]: d =>
      val integerPart = math.floor(d).toLong
      val decimalPart = ((d - integerPart) * 1e18).toLong
      (integerPart, decimalPart)

  def int: CoRand[Int] =
    CoRand.instance(identity)

  def long: CoRand[Long] =
    CoRand.instance(identity)

  def string: CoRand[String] =
    new CoRand[String](s => s.toList.traverse(char.rand).void)

  given ContravariantMonoidal[CoRand] with
    def contramap[A, B](fa: CoRand[A])(f: B => A): CoRand[B] =
      new CoRand(fa.rand.compose(f))
    override def product[A, B](fa: CoRand[A], fb: CoRand[B]): CoRand[(A, B)] =
      new CoRand((fa.rand *** fb.rand).map((a, b) => a >> b))
    override def unit: CoRand[Unit] =
      new CoRand(_ => Rand.const(()))
