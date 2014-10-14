(ns beer-song
  (:require [clojure.string :refer [capitalize join]]))

(defn- amount [n] (if (zero? n) "no more" (str n)))
(defn- bottles [n] (if (= 1 n) "bottle" "bottles"))
(defn- pronoun [n] (if (= 1 n) "it" "one"))
(defn- pred [n] (if (zero? n) 99 (dec n)))
(defn- action [n]
  (if (zero? n)
    "Go to the store and buy some more"
    (str "Take " (pronoun n) " down and pass it around")))


(defn- of-beer [n] (str (amount n) " " (bottles n) " of beer"))
(defn- on-the-wall [n] (str (of-beer n) " on the wall"))

(defn verse [n]
  (str (capitalize (on-the-wall n)) ", " (of-beer n) ".\n"
       (action n) ", " (on-the-wall (pred n)) ".\n"))

(defn sing
  ([start] (sing start 0))
  ([start finish]
    (let [verse-range (reverse (range finish (inc start)))]
      (join "\n" (map verse verse-range)))))
