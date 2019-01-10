defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(n) do
    """
    #{String.capitalize(on_the_wall(n))}, #{bottles_of_beer(n)}.
    #{action(n)}, #{on_the_wall(pred(n))}.
    """
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    Enum.map_join(range, "\n", &verse/1)
  end

  defp on_the_wall(n), do: "#{bottles_of_beer(n)} on the wall"

  defp bottles_of_beer(n), do: "#{amount(n)} #{bottles(n)} of beer"

  defp action(0), do: "Go to the store and buy some more"
  defp action(n), do: "Take #{pronoun(n)} down and pass it around"

  defp pred(0), do: 99
  defp pred(n), do: n - 1

  defp amount(0), do: "no more"
  defp amount(n), do: Integer.to_string(n)

  defp bottles(1), do: "bottle"
  defp bottles(_n), do: "bottles"

  defp pronoun(1), do: "it"
  defp pronoun(_n), do: "one"
end
