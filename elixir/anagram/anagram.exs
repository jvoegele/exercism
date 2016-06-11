defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t, [String.t]) :: [String.t]
  def match(base, candidates) do
    base = String.downcase(base)
    sorted_base = sort(base)
    Enum.filter(candidates,
      fn(candidate) ->
        String.downcase(candidate) != base and sort(candidate) == sorted_base
      end)
  end

  defp sort(string) do
    String.downcase(string)
    |> String.to_char_list
    |> Enum.sort
  end
end
