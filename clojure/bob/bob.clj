(ns bob (:require [clojure.string :as str]))

(defn question? [s] (= (last s) \?))

(defn yelling? [s]
  (letfn [(uppercase? [c] (Character/isUpperCase c))
          (at-least-one-uppercase? [s] (->> s (filter uppercase?) count pos?))]
    (and (at-least-one-uppercase? s) (= (str/upper-case s) s))))

(defn silence? [s] (= (str/trim s) ""))

(defn response-for [s]
  (cond
    (silence? s) "Fine. Be that way!"
    (yelling? s) "Whoa, chill out!"
    (question? s) "Sure."
    :else "Whatever."))

