package astrac.rand
package derivation

import cats.syntax.traverse.given
import cats.syntax.contravariant.given
import cats.syntax.contravariantSemigroupal.given
import scala.compiletime.*
import scala.deriving.Mirror

object DerivedAutoCoRand:
  inline def apply[A](using m: Mirror.Of[A]): AutoCoRand[A] =
    inline m match
      case s: Mirror.Sum     => deriveCoRandSum[A](using s)
      case p: Mirror.Product => deriveCoRandProduct[A](using p)

  // private inline def summonOrDeriveCoRand[A] =
  //   summonFrom:
  //     case a: AutoCoRand[A]     => a
  //     case m: Mirror.Of[A] => apply[A]

  private inline def deriveCoRandProduct[P](using
      m: Mirror.ProductOf[P]
  ): AutoCoRand[P] =
    inline erasedValue[m.MirroredElemTypes] match
      case _: (h *: t) =>
        val instances = summonAll[Tuple.Map[m.MirroredElemTypes, AutoCoRand]]
        val asCoRand =
          summonInline[Tuple.Union[instances.type] <:< AutoCoRand[?]]
        val coRands: List[CoRand[h *: t]] =
          instances.toList.mapWithIndex: (i, idx) =>
            val autoCoRand = asCoRand(i)
            autoCoRand.coRand.contramap[h *: t]: t =>
              t(idx).asInstanceOf[autoCoRand.coRand.In]
        AutoCoRand.instance:
          coRands
            .reduce((a, b) => (a, b).contramapN(a => (a, a)))
            .contramap(p =>
              m.fromProduct(p.asInstanceOf[P & Product]).asInstanceOf[h *: t]
            )
      case _: EmptyTuple => AutoCoRand.instance(CoRand(_ => Rand.skip))

  private inline def deriveCoRandSum[S](using
      m: Mirror.SumOf[S]
  ): AutoCoRand[S] =
    ???
