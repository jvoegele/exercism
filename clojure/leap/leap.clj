(ns leap)

(defn leap-year? [year]
  (letfn [(divisible? [divisor] (zero? (rem year divisor)))]
    (or (divisible? 400)
      (and (divisible? 4)
           (not (divisible? 100))))))
