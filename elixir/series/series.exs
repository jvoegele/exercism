defmodule StringSeries do
  @doc """
  Given a string `s` and a positive integer `size`, return all substrings
  of that size. If `size` is greater than the length of `s`, or less than 1,
  return an empty list.
  """
  @spec slices(s :: String.t(), size :: integer) :: list(String.t())
  def slices(s, size) do
    slices(s, size, []) |> Enum.reverse
  end

  defp slices(s, size, acc) do
    cond do
      size < 1 || String.length(s) < size ->
        acc
      true ->
        {slice, _rest} = String.split_at(s, size)
        slices(String.slice(s, 1..-1), size, [slice|acc])
    end
  end
end

