package astrac.rand
package randimpl

import cats.syntax.applicative.given
import cats.syntax.flatMap.given
import cats.syntax.functor.given

private[rand] transparent trait Combinators[A]:
  this: Rand[A] =>

  def dropIf(condition: A => Boolean): Rand[A] =
    this.flatMap: a =>
      if condition(a) then a.pure
      else
        for
          state <- Rand.state
          dropped = state.dropped + 1
          _ <- Rand.setState(state.copy(dropped = dropped))
          next <-
            if state.dropLimiter.shouldFail(state.iterations, dropped)
            then dropIf(condition)
            else DropLimiter.limitReached
        yield next
