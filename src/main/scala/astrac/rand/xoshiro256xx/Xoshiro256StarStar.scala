package astrac.rand
package xoshiro256xx

object Xoshiro256StarStar:

  def splitmix64_1(x: Long): Long =
    x + 0x9e3779b97f4a7c15L

  def splitmix64_2(z: Long): Long =
    val z1 = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9L
    val z2 = (z1 ^ (z1 >> 27)) * 0x94d049bb133111ebL
    z2 ^ (z2 >> 31);

  def rotl(x: Long, k: Int): Long =
    (x << k) | (x >>> (64 - k))

  def apply(vector: StateVector): (StateVector, Long) =
    val StateVector(s0, s1, s2, s3) = vector
    val result = rotl(s1 * 5, 7) * 9
    val t = s1 << 17
    val s2tmp = s2 ^ s0
    val s3tmp = s3 ^ s1
    val s1new = s1 ^ s2
    val s0new = s0 ^ s3tmp
    val s2new = s2tmp ^ t
    val s3new = rotl(s3tmp, 45)
    (StateVector(s0new, s1new, s2new, s3new), result)
