defmodule Pangram do
  @doc """
  Determines if a word or sentence is a pangram.
  A pangram is a sentence using every letter of the alphabet at least once.

  Returns a boolean.

    ## Examples

      iex> Pangram.pangram?("the quick brown fox jumps over the lazy dog")
      true

  """
  @spec pangram?(String.t) :: boolean
  def pangram?(sentence) do
    used_letters = sentence
      |> String.downcase
      |> to_charlist
      |> Enum.reduce(MapSet.new, fn(l, set) -> MapSet.put(set, l) end)
    Enum.all?(?a..?z, &(MapSet.member?(used_letters, &1)))
  end
end
