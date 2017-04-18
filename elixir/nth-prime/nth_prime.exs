defmodule Prime do

  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(count) when count > 0 do
    prime_stream() |> Enum.at(count - 1)
  end

  defp prime_stream() do
    Stream.iterate(2, &next_prime/1)
  end

  defp next_prime(n) do
    n = n + 1
    if prime?(n) do
      n
    else
      next_prime(n)
    end
  end

  defp prime?(n) when n < 2, do: false
  defp prime?(n) when n <= 3, do: true
  defp prime?(n) when rem(n, 2) == 0 or rem(n, 3) == 0, do: false
  defp prime?(n), do: prime?(n, 5)

  defp prime?(n, i) when i * i > n, do: true
  defp prime?(n, i) when rem(n, i) == 0 or rem(n, i + 2) == 0, do: false
  defp prime?(n, i), do: prime?(n, i + 6)
end
