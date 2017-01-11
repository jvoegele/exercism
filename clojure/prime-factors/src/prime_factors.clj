(ns prime-factors)

(defn- prime-factors [n candidate acc]
  (cond
    (< n candidate) acc
    (zero? (rem n candidate))
      (recur (/ n candidate) candidate (conj acc candidate))
    :else (recur n (inc candidate) acc)))

(defn of [n]
  (prime-factors n 2 []))

