package astrac.rand
package randimpl

import cats.syntax.applicative.given
import cats.syntax.flatMap.given
import cats.syntax.functor.given
import cats.syntax.traverse.given
import java.nio.ByteBuffer

private[rand] transparent trait Factories:
  this: Rand.type =>

  private val doubleMult = java.lang.Double.valueOf("0x1.0P-53")
  private val floatMult = java.lang.Float.valueOf("0x1.0P-24f")

  def boolean: Rand[Boolean] =
    long.map(l => (l & 1) != 0)

  // TODO: Improve this
  def bytes: Rand[Array[Byte]] =
    long.map(l => ByteBuffer.allocate(8).putLong(l).array())

  def char: Rand[Char] =
    long.map(_.toChar)

  def const[A](a: A): Rand[A] =
    a.pure

  def double: Rand[Double] =
    long.map(l => (l >>> 11) * doubleMult)

  def float: Rand[Float] =
    long.map(l => (l >>> 40) * floatMult)

  def fork: Rand[ForkRoot] =
    long.map(ForkRoot(_))

  def int: Rand[Int] =
    long.map(_.toInt)

  def list[A](count: Int, procA: Rand[A]): Rand[List[A]] =
    0.until(count).toList.traverse(_ => procA)

  def long: Rand[Long] =
    next

  def long(min: Long, max: Long): Rand[Long] = Rand.state.flatMap: s =>
    long.map(l => ((l & Long.MaxValue) % (max - min)) + min)

  def oneOf[A](procs: Rand[A]*): Rand[A] =
    long.flatMap(l => procs(math.abs(l.toInt) % procs.size))

  def option[A](proc: Rand[A]): Rand[Option[A]] =
    Rand.boolean.flatMap(b => if b then proc.map(Option(_)) else None.pure)

  def skip: Rand[Unit] =
    Rand.next.void

  // TODO: Make length generation and alphabet configurable
  def string: Rand[String] =
    long(0, 100).flatMap(l => list(l.toInt, char)).map(_.mkString)

  def vector[A](count: Int, procA: Rand[A]): Rand[Vector[A]] =
    0.until(count).toVector.traverse(_ => procA)
