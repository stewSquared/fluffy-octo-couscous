package challenge

import org.junit.Test
import org.junit.Assert._

class Test1 {
  /* Edge cases and properties to test:
   * [X] Order of appearance of pairs in each favorites list
   * [X] Order of appearance of favorites lists
   * [X] Results are normalized
   * [X] Threshold off by one
   * [ ] Duplicates in a favorites list
   * [X] Doubled user list == doubled occurrences
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

  def testDoubled(solution: Solution): Unit = {
    val pairs = solution.run(sample1 ++ sample1, threshold = 4)

    assertEquals(pairs, Seq(("funk1", "funk2")))
  }

  def testRandomized(solution: Solution): Unit = {
    val random = new util.Random

    val randomizedSamples = random.shuffle((1 to 10).flatMap(_ => sample1.map(random.shuffle)))

    val pairs = solution.run(randomizedSamples, threshold = 20)

    assertEquals(pairs, (Seq(("funk1", "funk2"))))
  }

  @Test def testBasicNaive = testBasic(naiveSolution)

  @Test def testBasicOptimized = testBasic(optimizedSolution)

  @Test def testDoubledNaive = testDoubled(naiveSolution)

  @Test def testDoubledOptimized = testDoubled(optimizedSolution)

  @Test def testRandomizedNaive = testRandomized(naiveSolution)

  @Test def testRandomizedOptimized = testRandomized(optimizedSolution)
}
