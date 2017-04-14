defmodule ProteinTranslation do
  @map %{
    "UGU" => "Cysteine",
    "UGC" => "Cysteine",
    "UUA" => "Leucine",
    "UUG" => "Leucine",
    "AUG" => "Methionine",
    "UUU" => "Phenylalanine",
    "UUC" => "Phenylalanine",
    "UCU" => "Serine",
    "UCC" => "Serine",
    "UCA" => "Serine",
    "UCG" => "Serine",
    "UGG" => "Tryptophan",
    "UAU" => "Tyrosine",
    "UAC" => "Tyrosine",
    "UAA" => "STOP",
    "UAG" => "STOP",
    "UGA" => "STOP"
  }

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: { atom,  list(String.t()) }
  def of_rna(rna) do
    candidate_proteins(rna) |> process_candidate_proteins()
  end

  @doc """
  Given a codon, return the corresponding protein

  UGU -> Cysteine
  UGC -> Cysteine
  UUA -> Leucine
  UUG -> Leucine
  AUG -> Methionine
  UUU -> Phenylalanine
  UUC -> Phenylalanine
  UCU -> Serine
  UCC -> Serine
  UCA -> Serine
  UCG -> Serine
  UGG -> Tryptophan
  UAU -> Tyrosine
  UAC -> Tyrosine
  UAA -> STOP
  UAG -> STOP
  UGA -> STOP
  """
  @spec of_codon(String.t()) :: { atom, String.t() }
  def of_codon(codon) do
    case Map.fetch(@map, codon) do
      :error -> {:error, "invalid codon"}
      {:ok, protein} -> {:ok, protein}
    end
  end

  defp candidate_proteins(rna) do
    rna
    |> to_charlist
    |> Enum.chunk(3)
    |> Enum.map(&(of_codon(to_string(&1))))
  end

  defp process_candidate_proteins(candidates) do
    case Enum.any?(candidates, fn({status, _}) -> status == :error end) do
      true -> {:error, "invalid RNA"}
      false ->
        result = candidates
          |> Enum.map(fn({:ok, protein}) -> protein end)
          |> Enum.take_while(fn(protein) -> protein != "STOP" end)
        {:ok, result}
    end
  end
end
