defmodule Minesweeper do

  @doc """
  Annotate empty spots next to mines with the number of mines next to them.
  """
  @spec annotate([String.t]) :: [String.t]

  def annotate([]), do: []
  def annotate(board) do
    board = Enum.map(board, fn(row) -> String.graphemes(row) end)
    Enum.map(0..(length(board) - 1), fn(row_num) ->
      process_row(board, row_num) |> Enum.join()
    end)
  end

  defp process_row(board, row_num) do
    row = Enum.at(board, row_num)
    row_with_indexes = Enum.with_index(row)
    Enum.reduce(row_with_indexes, [], fn({x, col_num}, acc) ->
      case x do
        "*" -> acc ++ [x]
        _ -> acc ++ [value_for_space(board, {col_num, row_num})]
      end
    end)
  end

  defp value_for_space(board, {x, y}) do
    case count_adjacent_bombs(board, {x, y}) do
      0 -> " "
      n -> n
    end
  end

  defp count_adjacent_bombs(board, {x, y}) do
    candidates = [{x-1, y-1}, {x, y-1}, {x+1, y-1},
                  {x-1, y  },           {x+1, y  },
                  {x-1, y+1}, {x, y+1}, {x+1, y+1}]
    Enum.reduce(candidates, 0, fn({x, y}, acc) ->
      case value_at(board, {x, y}) do
        "*" -> acc + 1
        _ -> acc
      end
    end)
  end

  defp value_at(_board, {x, y}) when x < 0 or y < 0, do: :error
  defp value_at(board, {x, y}) do
    try do
     Enum.fetch!(board, y) |> Enum.fetch!(x)
    rescue
      _ -> :error
    end
  end
end
