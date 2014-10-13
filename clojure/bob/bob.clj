(ns bob (:require [clojure.string :as str]))

(defn silence? [s] (= (str/trim s) ""))

(defn question? [s] (= (last s) \?))

(defn yelling? [s]
  (letfn [(at-least-one-uppercase? [s]
            (->> s
                 (filter #(Character/isUpperCase %))
                 count
                 pos?))]
    (and (at-least-one-uppercase? s)
         (= (str/upper-case s) s))))

(defn response-for [s]
  (cond
    (silence? s) "Fine. Be that way!"
    (yelling? s) "Whoa, chill out!"
    (question? s) "Sure."
    :else "Whatever."))

