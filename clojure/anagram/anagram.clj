(ns anagram (:require [clojure.string :refer [lower-case]]))

(defn anagram? [word candidate]
  (let [w (lower-case word)
        c (lower-case candidate)]
    (and (not= w c)
         (= (sort w) (sort c)))))

(defn anagrams-for [word candidates]
  (filter (partial anagram? word) candidates))
