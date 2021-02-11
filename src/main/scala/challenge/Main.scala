package challenge

def naiveSolution(userFavorites: Seq[Seq[String]], threshold: Int = 50): Seq[(String, String)] = {
  userFavorites
    .flatMap(artists => artists.sorted.combinations(2))
    .groupBy(identity)
    .toSeq
    .collect { case (Seq(a, b), occurrences) if occurrences.size >= threshold => (a, b) }
    .sorted
}

@main def findFrequentPairs(filename: String): Unit = {

  val userFavorites = io.Source.fromFile(filename).getLines
    .map(line => line.split(",").distinct.toSeq)

  val pairs = naiveSolution(userFavorites.toSeq)

  pairs.foreach { case (a, b) => println(s"$a,$b") }
}
