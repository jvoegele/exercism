(ns grains)

(defn- pow [x y]
  (.pow (biginteger x) y))

(defn square [n]
  (pow 2 (dec n)))

(defn total []
  (reduce + 0 (map square (range 1 65))))

