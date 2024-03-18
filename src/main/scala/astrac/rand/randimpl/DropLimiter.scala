package astrac.rand
package randimpl

import cats.syntax.applicative.given
import cats.syntax.flatMap.given
import cats.syntax.functor.given

private[rand] enum DropLimiter:
  case MaxCount(count: Long)
  case MaxRatio(ratio: Double)

  def shouldFail(iterations: Long, dropped: Long): Boolean =
    this match
      case MaxCount(count) => dropped >= count
      case MaxRatio(ratio) => (dropped.toDouble / iterations.toDouble) >= ratio

private[rand] object DropLimiter:
  def limitReached[A]: Rand[A] =
    Rand.state
      .map(_.dropped)
      .flatMap: dropped =>
        new Rand(Left(Rand.DropLimitReached(dropped)).pure)
