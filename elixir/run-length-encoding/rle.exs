defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "HORSE" => "1H1O1R1S1E"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "1H1O1R1S1E" => "HORSE"
  """
  @spec encode(String.t) :: String.t
  def encode(""), do: ""
  def encode(string) do
    char_list = String.to_char_list(string)
    initial_accumulator = [{List.first(char_list), 0}]
    Enum.reduce(char_list, initial_accumulator, &encode_char/2)
    |> Enum.reverse
    |> Enum.reduce("", &encode_run_length/2)
  end

  @spec decode(String.t) :: String.t
  def decode(string) do
    Regex.scan(~r/(\d+)(\w)/, string)
    |> Enum.reduce("", &decode_run_length/2)
  end

  @type run_length :: {char, non_neg_integer}

  @spec encode_char(char, [run_length, ...]) :: [run_length, ...]
  defp encode_char(char, [{last_char, count}|tail] = acc) do
    if char == last_char do
      [{char, count + 1}|tail]
    else
      [{char, 1}|acc]
    end
  end

  @spec encode_run_length(run_length, String.t) :: String.t
  defp encode_run_length({char, count}, acc) do
    acc <> to_string(count) <> <<char>>
  end

  defp decode_run_length([_match, count, char], acc) do
    acc <> String.duplicate(char, String.to_integer(count))
  end
end
