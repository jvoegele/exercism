class Phrase(phrase: String) {
  def wordCount: Map[String, Int] = {
    val words = phrase.toLowerCase.split("[^'\\w]").filter(_.matches(".*\\w.*"))
    words.foldLeft(Map[String, Int]().withDefaultValue(0)) { (acc, word) =>
      acc + (word -> (acc(word) + 1))
    }
  }
}
