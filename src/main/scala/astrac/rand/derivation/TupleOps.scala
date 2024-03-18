package astrac.rand
package derivation

import scala.compiletime.*
import cats.syntax.applicative.given
import cats.syntax.functor.given
import cats.syntax.semigroupal.given
import cats.Semigroupal
import cats.Functor
import cats.Applicative

object TupleOps:
  type Unmapped[F[_], A] = A match
    case F[a] => a

  type ConsF[F[_], H, T] = (H, T) match
    case (F[Unmapped[F, H]], F[Unmapped[F, T]]) =>
      F[Unmapped[F, H] *: (Unmapped[F, T] & Tuple)]

  inline def consF[F[_]: Semigroupal: Functor, H, T](
      a: H,
      t: T
  ): ConsF[F, H, T] =
    inline (a, t) match
      case fat: (F[Unmapped[F, H]], F[Unmapped[F, T]]) =>
        val asTuple = summonInline[Unmapped[F, T] =:= (Unmapped[F, T] & Tuple)]
        fat._1.product(fat._2).map((a, t) => a *: asTuple(t))

  inline def combineF[F[_]: Functor: Semigroupal, T <: Tuple](
      initial: F[EmptyTuple],
      t: T
  ): Sequence[F, T] =
    inline t match
      case x: (h *: t) =>
        consF(x.head[h *: t], combineF(initial, x.tail[h *: t]))
      case _: EmptyTuple => initial

  type Sequence[F[_], T] = T match
    case h *: t     => ConsF[F, h, Sequence[F, t]]
    case EmptyTuple => F[EmptyTuple]

  inline def sequence[F[_]: Applicative, T <: Tuple](
      t: T
  ): Sequence[F, T] =
    inline t match
      case x: (h *: t)     => consF(x.head[h *: t], sequence(x.tail[h *: t]))
      case x: (EmptyTuple) => EmptyTuple.pure[F]
