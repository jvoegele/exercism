defmodule ListOps do
  # Please don't use any external modules (especially List) in your
  # implementation. The point of this exercise is to create these basic functions
  # yourself.
  #
  # Note that `++` is a function from an external module (Kernel, which is
  # automatically imported) and so shouldn't be used either.

  @spec count(list) :: non_neg_integer
  def count(l), do: count(l, 0)

  defp count([], acc), do: acc
  defp count([_head|tail], acc), do: count(tail, acc + 1)

  @spec reverse(list) :: list
  def reverse([] = l), do: l
  def reverse([_] = l), do: l
  def reverse([a, b]), do: [b, a]
  def reverse([a, b | l]), do: reverse(l, [b, a])

  defp reverse([], x), do: x
  defp reverse([h|t], y), do: reverse(t, [h|y])

  @spec map(list, (any -> any)) :: list
  def map([], _f), do: []
  def map(l, f) do
    for i <- l, do: f.(i)
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], _f), do: []
  def filter(l, f) do
    for i <- l, f.(i), do: i
  end

  @type acc :: any
  @spec reduce(list, acc, ((any, acc) -> acc)) :: acc
  def reduce([], acc, _f), do: acc
  def reduce([head|tail], acc, f) do
    new_acc = f.(head, acc)
    reduce(tail, new_acc, f)
  end

  @spec append(list, list) :: list
  def append([], []), do: []
  def append([], [_|_] = l), do: l
  def append([_|_] = l, []), do: l
  def append(a, b) do
    reduce(reverse(a), b, fn(x, acc) -> [x|acc] end)
  end

  @spec concat([[any]]) :: [any]
  def concat([]), do: []
  def concat(ll) do
    reduce(reverse(ll), [], fn(l, acc) -> append(l, acc) end)
  end
end
