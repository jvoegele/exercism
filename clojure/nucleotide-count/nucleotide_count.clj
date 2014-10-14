(ns nucleotide-count)

(def dna-nucleotides #{\A \T \C \G})

(def all-nucleotides (conj dna-nucleotides \U))

(def count-zero (zipmap dna-nucleotides (repeat 0)))

(defn- validate! [nucleotide]
  (if-not (contains? all-nucleotides nucleotide)
    (throw (Exception. "invalid nucleotide"))))

(defn count [nucleotide dna]
  (validate! nucleotide)
  (clojure.core/count (filter #{nucleotide} dna)))

(defn nucleotide-counts [dna]
  (merge count-zero (frequencies dna)))
