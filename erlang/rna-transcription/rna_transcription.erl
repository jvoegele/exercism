-module(rna_transcription).
-export([to_rna/1]).

to_rna($G) -> $C;
to_rna($C) -> $G;
to_rna($T) -> $A;
to_rna($A) -> $U;
to_rna(Strand) ->
  lists:map(fun to_rna/1, Strand).
