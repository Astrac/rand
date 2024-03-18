package astrac.rand
package derivation

import scala.compiletime.*
import scala.deriving.Mirror
import cats.syntax.functor.given
import TupleOps.*

object DerivedAutoRand:
  inline def apply[A](using m: Mirror.Of[A]): AutoRand[A] =
    inline m match
      case s: Mirror.Sum     => deriveAutoRandSum[A](using s)
      case p: Mirror.Product => deriveAutoRandProduct[A](using p)

  private inline def summonOrDeriveAutoRand[A] =
    summonFrom:
      case a: AutoRand[A]  => a
      case m: Mirror.Of[A] => apply[A]

  private type SummonOrDeriveAutoRandsTuple[T <: Tuple] <: Tuple = T match
    case h *: t     => AutoRand[h] *: SummonOrDeriveAutoRandsTuple[t]
    case EmptyTuple => EmptyTuple

  private inline def summonOrDeriveAutoRandsTuple[T <: Tuple]
      : SummonOrDeriveAutoRandsTuple[T] =
    inline erasedValue[T] match
      case _: (h *: t) =>
        summonOrDeriveAutoRand[h] *: summonOrDeriveAutoRandsTuple[t]
      case _: EmptyTuple => EmptyTuple

  private inline def deriveAutoRandProduct[P](using
      m: Mirror.ProductOf[P]
  ): AutoRand[P] =
    type Sequenced =
      Sequence[AutoRand, SummonOrDeriveAutoRandsTuple[m.MirroredElemTypes]]
    val instances = summonOrDeriveAutoRandsTuple[m.MirroredElemTypes]
    val tupleAutoRand =
      sequence[AutoRand, SummonOrDeriveAutoRandsTuple[m.MirroredElemTypes]](
        instances
      )
    val asElemsF = summonInline[Sequenced =:= AutoRand[m.MirroredElemTypes]]
    asElemsF(tupleAutoRand).map(m.fromTuple)

  private inline def deriveAutoRandSum[S](using
      m: Mirror.SumOf[S]
  ): AutoRand[S] =
    AutoRand.instance[S]:
      val instances = summonOrDeriveAutoRandsTuple[m.MirroredElemTypes]
      val rands =
        autoRandsToRands[SummonOrDeriveAutoRandsTuple[m.MirroredElemTypes], S](
          instances
        ).toList
      val asRands =
        summonInline[Tuple.Union[
          AutoRandsToRands[SummonOrDeriveAutoRandsTuple[m.MirroredElemTypes], S]
        ] =:= Rand[S]]
      val procsEv = rands.map(asRands)
      Rand.oneOf(procsEv*)

  private type AutoRandToRand[AutoRandA, V] = AutoRandA match
    case AutoRand[Unmapped[AutoRand, AutoRandA]] => Rand[V]

  private inline def autoRandToRand[AutoRandA, V](
      a: AutoRandA
  ): AutoRandToRand[AutoRandA, V] =
    inline a match
      case autoRand: AutoRand[Unmapped[AutoRand, AutoRandA]] =>
        val asS = summonInline[Unmapped[AutoRand, AutoRandA] <:< V]
        autoRand.widen[Unmapped[AutoRand, AutoRandA]].map(asS).rand

  private type AutoRandsToRands[AutoRands, V] <: Tuple = AutoRands match
    case h *: t     => AutoRandToRand[h, V] *: AutoRandsToRands[t, V]
    case EmptyTuple => EmptyTuple

  private inline def autoRandsToRands[AutoRands, V](
      instances: AutoRands
  ): AutoRandsToRands[AutoRands, V] =
    inline instances match
      case is: (h *: t) =>
        autoRandToRand[h, V](is.head[h *: t]) *: autoRandsToRands[t, V](
          is.tail[h *: t]
        )
      case is: EmptyTuple => is
