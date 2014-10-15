(ns etl (:require [clojure.string :refer [lower-case]]))

(defn- map-vals-to-key [m k vs]
  (into m (zipmap (map lower-case vs) (repeat k))))

(defn transform [data]
  (reduce-kv map-vals-to-key {} data))

