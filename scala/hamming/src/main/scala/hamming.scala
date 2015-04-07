object Hamming {
  def compute(strand1: String, strand2: String) : Int =
    strand1.zip(strand2).foldLeft(0) { (acc, pair) =>
      acc + (if (pair._1 == pair._2) 0 else 1)
    }
}
