(ns triangle)

(defn valid-triangle? [s1 s2 s3]
  (let [[s1 s2 s3] (sort (vector s1 s2 s3))]
    (> (+ s1 s2) s3)))

(defn type [s1 s2 s3]
  (if (valid-triangle? s1 s2 s3)
    (case (count (hash-set s1 s2 s3))
      1 :equilateral
      2 :isosceles
      3 :scalene)
    :illogical))
