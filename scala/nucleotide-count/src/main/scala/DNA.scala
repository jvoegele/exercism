class DNA(string: String) {
  val DnaNucleotides = Set('A', 'T', 'C', 'G')
  val AllNucleotides = DnaNucleotides + 'U'

  require(string.forall(DnaNucleotides.contains(_)))

  def count(c: Char): Int = {
    require(AllNucleotides.contains(c))
    string count (_ == c)
  }

  def nucleotideCounts: Map[Char, Int] = {
    val CountZero: Map[Char, Int] = // William Gibson reference :)
      DnaNucleotides.zip(List.fill(4)(0)).toMap
    string.foldLeft(CountZero) { (acc, c) =>
      acc + (c -> (acc(c) + 1))
    }
  }
}
