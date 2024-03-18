package astrac.rand

import cats.kernel.Eq
import cats.laws.discipline.ContravariantMonoidalTests
import cats.syntax.apply.given
import cats.syntax.flatMap.given
import cats.syntax.traverse.given
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scala.deriving.Mirror

class CoRandLawsSuite extends DisciplineSuite:
  checkAll(
    "ContravariantMonoidal[CoRand]",
    ContravariantMonoidalTests[CoRand]
      .contravariantMonoidal[String, Int, Boolean]
  )

  given AutoCoRand[(String, Int, Boolean)] =
    new AutoCoRand((CoRand.string, CoRand.int, CoRand.boolean).tupled)

  given [A: AutoCoRand]: Arbitrary[CoRand[A]] =
    Arbitrary:
      Gen.long.map(l =>
        new CoRand(a => Rand.setSeed(l) >> AutoCoRand[A].coRand.rand(a))
      )

  given [A: AutoRand]: Eq[CoRand[A]] =
    Eq.instance: (aCoRand, bCoRand) =>
      val seed = Rand.next.unsafeRun()
      val as = Rand.list(20, AutoRand[A].rand).unsafeRun(seed)
      val (aCtx, _) = as.traverse(aCoRand.rand).unsafeRunWithState(seed)
      val (bCtx, _) = as.traverse(bCoRand.rand).unsafeRunWithState(seed)
      if aCtx != bCtx then println(s"aCtx: $aCtx\nbCtx: $bCtx")
      aCtx == bCtx
