defmodule PigLatin do
  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.

  Words beginning with consonants should have the consonant moved to the end of
  the word, followed by "ay".

  Words beginning with vowels (aeiou) should have "ay" added to the end of the
  word.

  Some groups of letters are treated like consonants, including "ch", "qu",
  "squ", "th", "thr", and "sch".

  Some groups are treated like vowels, including "yt" and "xr".
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split
    |> Enum.map(&translate_word/1)
    |> Enum.join(" ")
  end

  defp translate_word(word) do
    {leading, rest} = split(word)
    rest <> leading <> "ay"
  end

  defp split(word) do
    case starts_with_vowel_sound?(word) do
      true -> {"", word}
      false -> split_on_leading_consonant(word)
    end
  end

  @vowel_sound ~r/^(a|e|i|o|u)|(yt|xr)/

  defp starts_with_vowel_sound?(word) do
    Regex.match?(@vowel_sound, word)
  end

  @special_clusters ~r/^(ch|qu|squ|thr|th|sch)(.+)/

  defp split_on_leading_consonant(word) do
    # Assert that word does not start with vowel sound.
    false = starts_with_vowel_sound?(word)

    case Regex.run(@special_clusters, word) do
      nil ->
        String.split_at(word, 1)
      [_word, leading, rest] ->
        {leading, rest}
    end
  end
end

