defmodule Roman do
  @type place :: 1 | 10 | 100 | 1000
  @type numeral :: :I | :V | :X | :L | :C | :D | :M

  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    thousands(number) <> hundreds(number) <> tens(number) <> ones(number)
  end

  defp thousands(n) do
    duplicate(:M, place_value(n, 1000))
  end

  defp hundreds(n) do
    numerals_for_place(n, 100, :C, :D, :M)
  end

  defp tens(n) do
    numerals_for_place(n, 10, :X, :L, :C)
  end

  defp ones(n) do
    numerals_for_place(n, 1, :I, :V, :X)
  end

  @spec numerals_for_place(non_neg_integer, place, numeral, numeral, numeral) :: String.t()
  defp numerals_for_place(n, place, numeral1, numeral5, numeral10) do
    d = place_value(n, place)

    cond do
      d < 4 -> duplicate(numeral1, d)
      d == 4 -> "#{numeral1}#{numeral5}"
      d == 9 -> "#{numeral1}#{numeral10}"
      true -> "#{numeral5}#{duplicate(numeral1, d - 5)}"
    end
  end

  @spec place_value(pos_integer, place) :: integer
  defp place_value(n, place) , do: Integer.mod(div(n, place), 10)

  @spec duplicate(numeral, pos_integer) :: String.t()
  defp duplicate(numeral, n) do
    numeral
    |> to_string()
    |> String.duplicate(n)
  end
end
