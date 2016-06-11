defmodule Sublist do
  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) do
    a_size = Enum.count(a)
    b_size = Enum.count(b)
    cond do
      a == b ->
        :equal
      a_size < b_size ->
        if sublist?(a, b) do
          :sublist
        else
          :unequal
        end
      a_size > b_size ->
        if sublist?(b, a) do
          :superlist
        else
          :unequal
        end
      true ->
        :unequal
    end
  end

  defp sublist?(smaller, larger) when length(smaller) < length(larger) do
    smaller_size = Enum.count(smaller)
    Enum.any?(find_all_indexes(larger, List.first(smaller)),
      fn(index) ->
        Enum.slice(larger, index, smaller_size) == smaller
      end)
  end

  defp find_all_indexes(list, value) when is_list(list) do
    Enum.with_index(list)
    |> Enum.filter_map(
        fn({element, index}) -> element === value end,
        fn({element, index}) -> index end)
  end
end
