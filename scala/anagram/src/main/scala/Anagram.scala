class Anagram(word: String) {
  def matches(candidates: Seq[String]): Seq[String] =
    candidates filter (isAnagram(word, _))

  private def isAnagram(word: String, candidate: String): Boolean = {
    val w = word.toLowerCase
    val c = candidate.toLowerCase
    w != c && w.sorted == c.sorted
  }
}
