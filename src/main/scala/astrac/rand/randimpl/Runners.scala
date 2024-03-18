package astrac.rand
package randimpl

import astrac.rand.xoshiro256xx.StateVector

private[rand] transparent trait Runners[A]:
  this: Rand[A] =>

  def unsafeRun(seed: Long = System.currentTimeMillis()): A =
    run(seed).fold(throw _, identity)

  def run(seed: Long = System.currentTimeMillis()): Rand.Result[A] =
    state.runA(RandContext(StateVector.fromSeed(seed))).value

  def runWithState(
      seed: Long = System.currentTimeMillis()
  ): (RandContext, Rand.Result[A]) =
    state.run(RandContext(StateVector.fromSeed(seed))).value

  def unsafeRunWithState(
      seed: Long = System.currentTimeMillis()
  ): (RandContext, A) =
    val (ctx, res) = runWithState(seed)
    (ctx, res.fold(throw _, identity))
