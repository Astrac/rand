package astrac.rand

import astrac.rand.randimpl.DropLimiter
import astrac.rand.xoshiro256xx.StateVector

case class RandContext(
    vector: StateVector,
    dropLimiter: DropLimiter = DropLimiter.MaxRatio(0.1),
    iterations: Long = 0,
    dropped: Long = 0
)
