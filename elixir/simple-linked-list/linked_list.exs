defmodule LinkedList do
  @opaque t :: tuple()

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new() do
    {}
  end

  @doc """
  Push an item onto a LinkedList
  """
  @spec push(t, any()) :: t
  def push(list, elem) do
    {elem, list}
  end

  @doc """
  Calculate the length of a LinkedList
  """
  @spec length(t) :: non_neg_integer()
  def length(list) do
    length(list, 0)
  end

  defp length({}, acc), do: acc
  defp length({_, tail}, acc) do
    length(tail, acc + 1)
  end

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?({}), do: true
  def empty?(_list), do: false

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek({}), do: {:error, :empty_list}
  def peek({value, _}) do
    {:ok, value}
  end

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail({}), do: {:error, :empty_list}
  def tail({_, tail}) do
    {:ok, tail}
  end

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop({}), do: {:error, :empty_list}
  def pop({value, tail}) do
    {:ok, value, tail}
  end

  @doc """
  Construct a LinkedList from a stdlib List
  """
  @spec from_list(list()) :: t
  def from_list(list) do
    from_list(Enum.reverse(list), {})
  end

  defp from_list([], acc), do: acc
  defp from_list([head|tail], acc) do
    from_list(tail, {head, acc})
  end

  @doc """
  Construct a stdlib List LinkedList from a LinkedList
  """
  @spec to_list(t) :: list()
  def to_list(list) do
    Enum.reverse(to_list(list, []))
  end

  defp to_list({}, acc), do: acc
  defp to_list({value, tail}, acc) do
    to_list(tail, [value|acc])
  end

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list) do
    reverse(list, {})
  end

  defp reverse({}, acc), do: acc
  defp reverse({value, next}, acc) do
    reverse(next, {value, acc})
  end
end
