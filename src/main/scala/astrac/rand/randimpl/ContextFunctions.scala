package astrac.rand
package randimpl

import astrac.rand.xoshiro256xx.StateVector
import cats.data.State
import cats.syntax.applicative.given
import cats.syntax.flatMap.given
import cats.syntax.functor.given
import astrac.rand.xoshiro256xx.Xoshiro256StarStar

private[rand] transparent trait ContextFunctions:
  def jump: Rand[Unit] =
    state.flatMap: state =>
      val jumps = Array(
        0x180ec6d33cfd0abaL,
        0xd5a61266f0c9392cL,
        0xa9582618e03fc9aaL,
        0x39abdc4529b1661cL
      )

      val v = state.vector
      var s0 = 0L
      var s1 = 0L
      var s2 = 0L
      var s3 = 0L
      jumps.foreach: j =>
        0L.until(64L)
          .foreach: b =>
            if (j & 1L << b) != 0 then
              s0 = s0 ^ v.s0
              s1 = s1 ^ v.s1
              s2 = s2 ^ v.s2
              s3 = s3 ^ v.s3
      setVector(StateVector(s0, s1, s2, s3)) >> Rand.skip

  def state: Rand[RandContext] =
    new Rand(State.get.map(_.pure))

  def setSeed(s: Long): Rand[Unit] =
    setVector(StateVector.fromSeed(s))

  def setVector(v: StateVector): Rand[Unit] =
    new Rand(State.modify[RandContext](_.copy(vector = v)).map(_.pure))

  def setState(s: RandContext): Rand[Unit] =
    new Rand(State.set(s).map(_.pure))

  def next: Rand[Long] =
    for
      s <- state
      (newVector, value) = Xoshiro256StarStar(s.vector)
      newIt = s.iterations + 1
      newS = s.copy(iterations = newIt, vector = newVector)
      _ <- Rand.setState(newS)
    yield value
