defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t
  def numerals(number) do
    thousands(number) <> hundreds(number) <> tens(number) <> ones(number)
  end

  defp thousands(n) do
    String.duplicate("M", digit_in_place(n, 1000))
  end

  @doc """
  Returns the digit in the specified `place` for the given `number`.

  ## Examples

    iex> Roman.digit_in_place(4321, 1000)
    4
    iex> Roman.digit_in_place(4321, 100)
    3
    iex> Roman.digit_in_place(4321, 10)
    2
    iex> Roman.digit_in_place(4321, 1)
    1
  """
  def digit_in_place(number, place) do
    rem(div(number, place), 10)
  end
end
