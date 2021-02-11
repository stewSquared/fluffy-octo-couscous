package challenge

import org.junit.Test
import org.junit.Assert._

class Test1 {
  /* Edge cases and properties to test:
   * [X] Order of appearance of pairs in each favorites list
   * [ ] Order of appearance of favorites lists
   * [ ] Results are normalized
   * [X] Threshold off by one
   * [ ] Duplicates in a favorites list
   * [ ] Doubled user list == doubled occurrences
   */

  val sample1 = Seq(
    Seq("funk1", "funk2", "rock"),
    Seq("pop", "funk2", "funk1"),
    Seq("pop", "rock", "country")
  )

  type Solution = (Seq[Seq[String]], Int) => Seq[(String, String)]

  extension (s: Solution) {
    def run(userFavorites: Seq[Seq[String]], threshold: Int) =
      s(userFavorites, threshold)
  }

  def testBasic(solution: Solution): Unit = {
    val pairsAppearingAtLeastTwice = solution.run(sample1, threshold = 2)

    assertEquals(pairsAppearingAtLeastTwice, Seq(("funk1", "funk2")))

    val allPairs = solution.run(sample1, threshold = 1)

    assertEquals(allPairs.sorted,
      Seq(
        ("country", "pop"),
        ("country", "rock"),
        ("funk1", "funk2"),
        ("funk1", "pop"),
        ("funk1", "rock"),
        ("funk2", "pop"),
        ("funk2", "rock"),
        ("pop", "rock")
      )
    )
  }

  @Test def testBasicNaive: Unit = {
    testBasic(naiveSolution)
  }

  @Test def testBasicOptimized: Unit = {
    testBasic(optimizedSolution)
  }
}
