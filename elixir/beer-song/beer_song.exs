defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t
  def verse(number) do
    n = number - 1
    String.capitalize(on_the_wall(n)) <> ", " <> bottles_of_beer(n) <> ".\n" <>
    action(n) <> ", " <> on_the_wall(pred(n)) <> ".\n"
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t) :: String.t
  def lyrics(range \\ 100..1) do
    Enum.map_join(range, "\n", &verse/1)
  end

  def on_the_wall(n), do: bottles_of_beer(n) <> " on the wall"

  def bottles_of_beer(n), do: amount(n) <> " " <> bottles(n) <> " of beer"

  def action(0), do: "Go to the store and buy some more"
  def action(n), do: "Take " <> pronoun(n) <> " down and pass it around"

  def pred(0), do: 99
  def pred(n), do: n - 1

  def amount(0), do: "no more"
  def amount(n), do: Integer.to_string(n)

  def bottles(1), do: "bottle"
  def bottles(_n), do: "bottles"

  def pronoun(1), do: "it"
  def pronoun(_n), do: "one"
end
