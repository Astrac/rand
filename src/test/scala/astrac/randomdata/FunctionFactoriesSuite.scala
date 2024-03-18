package astrac.rand

import cats.syntax.apply.given
import munit.FunSuite
import scala.annotation.experimental

@experimental
class FunctionFactoriesSuite extends FunSuite:
  test("Generates a pure Function0"):
    val function = AutoRand[() => Long].rand.unsafeRun()
    val firstCall = function()
    val secondCall = function()
    assertEquals(firstCall, secondCall)

  test("Generates a pure Function1"):
    val function = AutoRand[Int => Long].rand.unsafeRun()
    val ints = Rand.list(10, AutoRand[Int].rand).unsafeRun()
    val firstCalls = ints.map(function)
    val secondCalls = ints.map(function)
    assertEquals(firstCalls, secondCalls)

  test("Generates a pure Function2"):
    val function = AutoRand[(Int, Int) => Long].rand.unsafeRun()
    val listRand = Rand.list(10, AutoRand[Int].rand)
    val ints = (listRand, listRand).mapN(_.zip(_)).unsafeRun()
    val firstCalls = ints.map(function.tupled)
    val secondCalls = ints.map(function.tupled)
    assertEquals(firstCalls, secondCalls)
