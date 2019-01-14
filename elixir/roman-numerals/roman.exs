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

  defguardp is_place(n) when n in [1, 10, 100, 1000]

  defguardp is_numeral(numeral) when numeral in ~w[I V X L C D M]a

  defguardp is_roman_number(n) when is_integer(n) and n > 0 and n <= 3000

  defp thousands(n) when is_roman_number(n) do
    duplicate(:M, place_value(n, 1000))
  end

  defp hundreds(n) when is_roman_number(n) do
    numerals_for_place(n, 100, :C, :D, :M)
  end

  defp tens(n) when is_roman_number(n) do
    numerals_for_place(n, 10, :X, :L, :C)
  end

  defp ones(n) when is_roman_number(n) do
    numerals_for_place(n, 1, :I, :V, :X)
  end

  @spec numerals_for_place(non_neg_integer, place, numeral, numeral, numeral) :: String.t()
  defp numerals_for_place(n, place, numeral1, numeral5, numeral10)
       when is_roman_number(n) and is_place(place) and is_numeral(numeral1) and
              is_numeral(numeral5) and is_numeral(numeral10) do
    d = place_value(n, place)

    cond do
      d < 4 -> duplicate(numeral1, d)
      d == 4 -> "#{numeral1}#{numeral5}"
      d == 9 -> "#{numeral1}#{numeral10}"
      true -> "#{numeral5}#{duplicate(numeral1, d - 5)}"
    end
  end

  @spec place_value(pos_integer, place) :: integer
  defp place_value(n, place) when is_roman_number(n) and is_place(place),
    do: Integer.mod(div(n, place), 10)

  @spec duplicate(numeral, pos_integer) :: String.t()
  defp duplicate(numeral, n) when is_numeral(numeral) and is_integer(n) do
    numeral
    |> to_string()
    |> String.duplicate(n)
  end
end
