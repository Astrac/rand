package astrac.rand

import astrac.rand.xoshiro256xx.StateVector
import cats.syntax.flatMap.given

class ForkRoot(seed: Long):
  private def vector: StateVector =
    StateVector.fromSeed(seed)

  def use[A](proc: Rand[A]): Rand[A] =
    Rand.setVector(vector) >> proc
