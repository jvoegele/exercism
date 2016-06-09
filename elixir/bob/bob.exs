defmodule Teenager do
  def hey(input) do
    cond do
      question?(input) -> "Sure."
      shouting?(input) -> "Whoa, chill out!"
      silence?(input) -> "Fine. Be that way!"
      :else -> "Whatever."
    end
  end

  defp question?(input), do: String.ends_with?(input, "?")

  defp silence?(input), do: String.strip(input) == ""

  defp shouting?(input) do
    String.upcase(input) == input and String.downcase(input) != input
  end
end
