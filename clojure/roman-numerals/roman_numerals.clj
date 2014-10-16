(ns roman-numerals)

(defn- str*
  "Produce a new string containing n copies of s."
  [s n]
  (apply str (repeat n s)))

(defn- place-value [x place] (mod (quot x place) 10))

(defn- thousands [x]
  (str* "M" (place-value x 1000)))

(defn- numerals-for-place [x place numeral1 numeral5 numeral10]
  (let [d (place-value x place)]
    (cond
      (< d 4) (str* numeral1 d)
      (= d 4) (str numeral1 numeral5)
      (= d 9) (str numeral1 numeral10)
      :else   (str numeral5 (str* numeral1 (- d 5))))))

(defn- hundreds [x]
  (numerals-for-place x 100 "C" "D" "M"))

(defn- tens [x]
  (numerals-for-place x 10 "X" "L" "C"))

(defn- ones [x]
  (numerals-for-place x 1 "I" "V" "X"))

(defn numerals [x]
  (str (thousands x) (hundreds x) (tens x) (ones x)))

