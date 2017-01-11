defmodule PrimeFactors do
  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(number) do
    Enum.reverse(factors_for(number, 2, []))
  end

  defp factors_for(number, candidate, acc) do
    cond do
      number < candidate ->
        acc
      rem(number, candidate) == 0 ->
        factors_for(div(number, candidate), candidate, [candidate|acc])
      :else ->
        factors_for(number, candidate + 1, acc)
    end
  end
end
