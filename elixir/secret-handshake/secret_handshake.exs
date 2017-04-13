defmodule SecretHandshake do
  @actions ["jump", "close your eyes", "double blink", "wink"]

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    [reverse_flag|action_flags] = binary_digits(code)
    zipped = Enum.zip(@actions, action_flags)
    actions = Enum.reduce(zipped, [], &maybe_add_action/2)
    case reverse_flag do
      ?0 -> actions
      ?1 -> Enum.reverse(actions)
    end
  end

  defp binary_digits(num) do
    num
    |> Integer.to_string(2)
    |> String.pad_leading(5, "0")
    |> String.slice(-5..-1)
    |> to_charlist
  end

  defp maybe_add_action({action, ?0}, acc), do: acc
  defp maybe_add_action({action, ?1}, acc), do: [action|acc]
end

