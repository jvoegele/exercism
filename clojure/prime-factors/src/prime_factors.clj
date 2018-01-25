(ns prime-factors)

;; (defn- prime? [number]
;;   (.isProbablePrime (biginteger number) 2))
;;
;; (defn- next-candidate [candidate max-candidate]
;;   (let [candidates (range candidate max-candidate)]
;;     (or (some #(when (prime? %) %) candidates)
;;         max-candidate)))
;;
(defn of [number]
  (loop [n number
         candidate 2
         acc []]
    (cond
      (< n candidate) acc
      (zero? (rem n candidate))
        (recur (/ n candidate) candidate (conj acc candidate))
      :else (recur n (inc candidate) acc))))

