(ns word-count
  (:require [clojure.string :refer [split lower-case]]))

(defn word-count [string]
  (-> string
      lower-case
      (split #"\W+")
      frequencies))
