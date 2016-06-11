defmodule BracketPush do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t) :: boolean
  def check_brackets(str) do
    remaining = Enum.reduce_while(String.to_char_list(str), [],
      fn(c, stack) ->
        case accept?(c, stack) do
          {true, new_stack} ->
            {:cont, new_stack}
          {false, _} ->
            {:halt, [false]}
        end
      end)
    [] == remaining
  end

  defp accept?(char, stack) do
    cond do
      open_bracket?(char) ->
        {true, [char|stack]}
      close_bracket?(char) ->
        match_top(stack, char)
      true ->
        {true, stack}
    end
  end

  defp open_bracket?(char) do
    char == ?( or char == ?[ or char == ?{
  end

  defp close_bracket?(char) do
    char == ?) or char == ?] or char == ?}
  end

  defp match_top([], _char), do: {false, []}
  defp match_top(stack, char) do
    [top|rest] = stack
    if (top == ?( and char == ?)) or
       (top == ?[ and char == ?]) or
       (top == ?{ and char == ?}) do
      {true, rest}
    else
      {false, stack}
    end
  end
end
