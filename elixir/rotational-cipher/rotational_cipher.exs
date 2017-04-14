defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    text
    |> to_charlist
    |> Enum.map(&(rotate_char(&1, shift)))
    |> to_string
  end

  defp rotate_char(char, shift) when char in ?A..?Z,
    do: rotate_char(char, shift, ?A..?Z)
  defp rotate_char(char, shift) when char in ?a..?z,
    do: rotate_char(char, shift, ?a..?z)
  defp rotate_char(char, _),
    do: char

  defp rotate_char(char, shift, range) do
    shifted = char + shift
    if shifted <= range.last do
      shifted
    else
      range.first + shifted - range.last - 1
    end
  end
end

