package astrac.rand
package xoshiro256xx

import Xoshiro256StarStar.splitmix64_1
import Xoshiro256StarStar.splitmix64_2

case class StateVector(s0: Long, s1: Long, s2: Long, s3: Long)

object StateVector:
  def fromSeed(seed: Long): StateVector =
    val sms0 = splitmix64_1(seed)
    val s0 = splitmix64_2(sms0)
    val sms1 = splitmix64_1(sms0)
    val s1 = splitmix64_2(sms1)
    val sms2 = splitmix64_1(sms1)
    val s2 = splitmix64_2(sms2)
    val sms3 = splitmix64_1(sms2)
    val s3 = splitmix64_2(sms3)
    StateVector(s0, s1, s2, s3)
