(ns phone-number)

(def invalid "0000000000")

(defn number [text]
  (let [digits (clojure.string/join (re-seq #"\d" text))
        digits-count (count digits)]
    (cond
      (< digits-count 10) invalid
      (= digits-count 10) digits
      :else
        (if (.startsWith digits "1")
          (number (subs digits 1))
          invalid))))

(defn area-code [text] (subs (number text) 0 3))

(defn pretty-print [text]
  (let [num (number text)]
    (str "(" (area-code num) ") " (subs num 3 6) "-" (subs num 6))))
