defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    Enum.sum(multiples(limit, factors))
  end

  defp multiples(limit, factors) do
    for x <- 1..(limit - 1), multiple?(x, factors), do: x
  end

  defp multiple?(n, factors) do
    Enum.any?(factors, &(rem(n, &1) == 0))
  end
end
