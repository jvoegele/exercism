(ns robot-name)

(defn- random-char []
  (char (+ 65 (rand-int 26))))

(defn- random-name []
  (str (random-char)
       (random-char)
       (rand-int 9)
       (rand-int 9)
       (rand-int 9)))

(defn robot [] (atom (random-name)))
(defn robot-name [ref] @ref)
(defn reset-name [ref] (swap! ref (fn [_] (random-name))))

