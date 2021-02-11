package challenge

import org.junit.Test
import org.junit.Assert._

class Test1 {
  @Test def testNaive(): Unit = {
    def sample = Seq(
      Seq("funk1", "funk2", "rock"),
      Seq("pop", "funk2", "funk1"),
      Seq("pop", "rock", "country")
    )

    val pairsAppearingAtLeastTwice = naiveSolution(sample, threshold = 2)

    assertEquals(pairsAppearingAtLeastTwice, Seq(("funk1", "funk2")))

    val allPairs = naiveSolution(sample, threshold = 1)

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
}
