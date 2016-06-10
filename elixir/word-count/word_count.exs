defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t) :: map() 
  def count(sentence) do
    sentence
    |> String.downcase
    |> String.split(~r/[^\w-]|[_]/u, trim: true)
    |> frequencies
  end

  defp frequencies(words) do
    Enum.reduce(words, Map.new,
      fn(w, acc) ->
        Map.update(acc, w, 1, &(&1 + 1))
      end)
  end
end
