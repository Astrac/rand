package astrac.rand

import astrac.rand.randimpl.*
import cats.Monad
import cats.data.State
import cats.syntax.applicative.given
import cats.syntax.flatMap.given
import cats.syntax.functor.given
import cats.syntax.traverse.given

// TODO: make state protected
class Rand[A](val state: State[RandContext, Rand.Result[A]])
    extends Runners[A]
    with Combinators[A]

object Rand
    extends ContextFunctions
    with Errors
    with Factories
    with FunctionFactories:
  type Result[A] = Either[Rand.Error, A]

  given Monad[Rand] with
    override def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] =
      Rand(fa.state.flatMap(_.flatTraverse(f.map(_.state))))
    override def tailRecM[A, B](a: A)(f: A => Rand[Either[A, B]]): Rand[B] =
      Rand(a.tailRecM(f.map(p => p.state.map(_.sequence))))
    override def pure[A](a: A): Rand[A] =
      Rand(Right(a).pure)
