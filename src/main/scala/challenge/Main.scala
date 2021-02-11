package challenge

def naiveSolution(userFavorites: Seq[Seq[String]]): Seq[(String, String)] = {
  userFavorites
    .flatMap(artists => artists.sorted.combinations(2))
    .groupBy(identity)
    .toSeq
    .collect { case (Seq(a, b), occurrences) if occurrences.size >= 50 => (a, b) }
    .sorted
}

@main def findFrequentPairs(filename: String): Unit = {

  val userFavorites = io.Source.fromFile(filename).getLines
    .map(line => line.split(",").distinct.toSeq)

  val pairs = naiveSolution(userFavorites.toSeq)

  pairs.foreach { case (a, b) => println(s"$a,$b") }
}
