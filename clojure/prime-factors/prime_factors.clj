(ns prime-factors)

(defn prime?
  "Is n a prime number?"
  [n]
  (let [certainty 5]
    (.isProbablePrime (biginteger n) certainty)))

(def primes
  "Lazy sequence of prime numbers."
  (filter prime? (range)))

(defn divisor?
  "Is y a divisor of x?"
  [x y]
  (zero? (rem x y)))

(defn of [n]
  (loop [primes primes
         result []]))

