package astrac.rand

import munit.DisciplineSuite
import cats.laws.discipline.MonadTests
import cats.syntax.all.given
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.kernel.Eq
import scala.annotation.experimental

@experimental
class RandLawsSuite extends DisciplineSuite:
  checkAll("Monad[Rand]", MonadTests[Rand].monad[String, Int, Boolean])

  given [A: AutoRand]: Arbitrary[Rand[A]] =
    Arbitrary(Gen.long.map(AutoRand[A].rand.unsafeRun(_).pure))

  given [A: Eq]: Eq[Rand[A]] =
    Eq.instance: (aRand, bRand) =>
      val seed = Rand.next.unsafeRun()
      val as = 0.to(20).toList.traverse(_ => aRand).unsafeRun(seed)
      val bs = 0.to(20).toList.traverse(_ => bRand).unsafeRun(seed)
      Eq[List[A]].eqv(as, bs)
