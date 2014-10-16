(ns binary)

(defn- extract-bits [binary]
  (if (every? #{\0 \1} binary)
    (map #(if (= % \1) 1 0) binary)
    [0]))

(defn- bit-value [bit pos]
  (int (* bit (Math/pow 2 pos))))

(defn to-decimal [binary]
  (loop [bits (reverse (extract-bits binary))
         result 0
         pos 0]
    (if (empty? bits)
      result
      (recur (rest bits)
             (+ result (bit-value (first bits) pos))
             (inc pos)))))

