(ns rna-transcription)

(def dna->rna
  {\G \C
   \C \G
   \T \A
   \A \U})

(defn to-rna [dna]
  {:pre [(every? #{\A \C \G \T} dna)]}
  (clojure.string/join (map dna->rna dna)))
