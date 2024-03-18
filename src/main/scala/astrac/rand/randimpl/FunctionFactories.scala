package astrac.rand
package randimpl

import cats.syntax.all.given
import scala.util.TupledFunction
import scala.annotation.experimental

private[rand] transparent trait FunctionFactories:
  this: Rand.type =>

  def function1[A, B](aCoRand: CoRand[A], bRand: Rand[B]): Rand[A => B] =
    for
      seed <- Rand.long
      // TODO: Investigate replacing `unsafeRun` with `run`
      fn <- Rand.const((a: A) => (aCoRand.rand(a) >> bRand).unsafeRun(seed))
    yield fn

  def function2[A, B, C](
      aCoRand: CoRand[A],
      bCoRand: CoRand[B],
      cRand: Rand[C]
  ): Rand[(A, B) => C] =
    for
      seed <- Rand.long
      // TODO: Investigate replacing `unsafeRun` with `run`
      fn <- Rand.const((a: A, b: B) =>
        (aCoRand.rand(a) >> bCoRand.rand(b) >> cRand).unsafeRun(seed)
      )
    yield fn

  @experimental
  def function[F, Args <: Tuple, R](argsCoRand: CoRand[Args], rRand: Rand[R])(
      using t: TupledFunction[F, Args => R]
  ): Rand[F] =
    for
      seed <- Rand.long
      // TODO: Investigate replacing `unsafeRun` with `run`
      fn <- Rand.const((args: Args) =>
        (argsCoRand.rand(args) >> rRand).unsafeRun(seed)
      )
    yield t.untupled(fn)
