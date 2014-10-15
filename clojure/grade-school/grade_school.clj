(ns grade-school)

(defn grade [db n](get db n []))

(defn add [db name n]
  (assoc db n (conj (grade db n) name)))

(defn sorted [db]
  (reduce-kv (fn [ret k v] (assoc ret k (sort v)))
             (sorted-map)
             db))

