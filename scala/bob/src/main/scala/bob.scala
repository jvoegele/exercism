class Bob {
  def hey(input: String): String =
    if (isSilence(input))
      "Fine. Be that way!"
    else if (isShouting(input))
      "Whoa, chill out!"
    else if (isQuestion(input))
      "Sure."
    else
      "Whatever."

  private def isShouting(s: String) : Boolean =
    s.exists(Character.isUpperCase(_)) && !s.exists(Character.isLowerCase(_))

  private def isQuestion(s: String) : Boolean =
    s.endsWith("?")

  private def isSilence(s: String) : Boolean =
    s.trim.isEmpty
}
