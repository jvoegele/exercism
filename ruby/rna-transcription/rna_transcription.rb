class Complement
  def self.of_dna(nucleotide)
    nucleotide.chars.map { |c| dna_to_rna(c) }.join
  end

  def self.of_rna(nucleotide)
    nucleotide.chars.map { |c| rna_to_dna(c) }.join
  end

  private

  DNA_NUCLEOTIDES = %w[G C T A]
  RNA_NUCLEOTIDES = %w[C G A U]
  DNA_TO_RNA = Hash[DNA_NUCLEOTIDES.zip(RNA_NUCLEOTIDES)]
  RNA_TO_DNA = DNA_TO_RNA.invert

  def self.dna_to_rna(nucleotide)
    raise ArgumentError unless DNA_NUCLEOTIDES.include?(nucleotide)
    DNA_TO_RNA[nucleotide]
  end

  def self.rna_to_dna(nucleotide)
    raise ArgumentError unless RNA_NUCLEOTIDES.include?(nucleotide)
    RNA_TO_DNA[nucleotide]
  end
end
