(ns scrabble-score
  (:require [clojure.string :refer [upper-case]]))

(defn- string-contains?
  "Does the string contain the given letter?"
  [string letter]
  ((set string) letter))

(defn score-letter [letter]
  ;; This function might be called with a string argument (from the test)
  ;; or called with a character (from score-word), so handle either case.
  (let [c (cond (char? letter) letter
                (string? letter) (first letter))]
    (condp string-contains? (Character/toUpperCase c)
      "AEIOULNRST" 1
      "DG" 2
      "BCMP" 3
      "FHVWY" 4
      "K" 5
      "JX" 8
      "QZ" 10)))

(defn score-word [word]
  (reduce + 0 (map score-letter word)))

