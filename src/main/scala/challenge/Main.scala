package challenge

/**
  * This naive solution will work on 1000 lines in under one second.
  * Its implementation is simple, so it's useful for validating a more optimized solution.
  */
def naiveSolution(userFavorites: Seq[Seq[String]], threshold: Int): Seq[(String, String)] = {
  userFavorites
    .flatMap(artists => artists.sorted.combinations(2))
    .groupBy(identity)
    .toSeq
    .collect { case (Seq(a, b), occurrences) if occurrences.size >= threshold => (a, b) }
    .sorted
}

/**
  * This solution is more complicated, but is written with more room
  * for optimization.
  */
def optimizedSolution(userFavorites: Seq[Seq[String]], threshold: Int): Seq[(String, String)] = {
  val artistOccurrences: Map[String, Set[Int]] = {
    val mutableMap = collection.mutable.Map.empty[String, Set[Int]]
      .withDefault(_ => Set())

    userFavorites.zipWithIndex.foreach { (artists, index) =>
      artists.foreach { artist =>
        mutableMap(artist) += index
      }
    }

    // This filter has room for probabilistic optimization.  e.g. an
    // artist with 51 only occurences overall isn't likely to appear
    // with any other artist 50 times. However, even this check as-is
    // causes this solution to run faster than naiveSolution.
    mutableMap
      .filter { case (artist, userIndices) => userIndices.size >= threshold}
      .toMap
  }

  def appearsFrequently(artist1: String, artist2: String): Boolean = {
    // This predicate has room for short-circuiting optimization.
    artistOccurrences(artist1)
      .intersect(artistOccurrences(artist2))
      .size >= threshold
  }

  artistOccurrences.keys.toSeq.sorted.combinations(2)
    .collect { case Seq(a, b) if appearsFrequently(a, b)=> (a, b) }
    .toSeq
    .sorted
}

@main def findFrequentPairs(filename: String): Unit = {

  val userFavorites = io.Source.fromFile(filename).getLines
    .map(line => line.split(",").distinct.toSeq)

  val pairs = optimizedSolution(userFavorites.toSeq, threshold = 50)

  pairs.foreach { case (a, b) => println(s"$a,$b") }
}
