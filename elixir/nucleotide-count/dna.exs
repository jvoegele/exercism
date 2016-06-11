defmodule DNA do
  @nucleotides [?A, ?C, ?G, ?T]

  # COUNT ZERO INTERRUPT - on receiving an interrupt, decrement the counter to zero.
  @count_zero %{?A => 0, ?C => 0, ?G => 0, ?T => 0}

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> DNA.count('AATAA', ?A)
  4

  iex> DNA.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide) do
    validate_nucleotide!(nucleotide)
    validate_strand!(strand)
    Enum.filter(strand, &(&1 == nucleotide)) |> Enum.count
  end


  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> DNA.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand) do
    validate_strand!(strand)
    Enum.reduce(strand, @count_zero,
      fn(nucleotide, acc) ->
        Map.update(acc, nucleotide, 0, &(&1 + 1))
      end)
  end

  defp validate_nucleotide!(nucleotide) do
    unless Enum.member?(@nucleotides, nucleotide) do
      raise ArgumentError
    end
  end

  defp validate_strand!(strand) do
    Enum.all?(strand, &validate_nucleotide!/1)
  end
end
