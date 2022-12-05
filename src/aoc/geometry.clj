(ns aoc.geometry)

(defn- transform-relative [p origin tx]
  (-> p
      (sub origin)
      tx
      (add origin)))

(defn rotate-left "Y axis points down"
  ([[x y]] [y (- x)])
  ([p center] (transform-relative p center rotate-left)))

(defn rotate-right "Y axis points down"
  ([[x y]] [(- y)  x])
  ([p center] (transform-relative p center rotate-right)))

(defn flip-vert "Y axis points down"
  ([[x y]] [(- x) y])
  ([p [cx cy]] (transform-relative p [cx 0] flip-vert)))

(defn flip-horiz "Y axis points down"
  ([[x y]] [x (- y)])
  ([p [cx cy]] (transform-relative p [0 cy] flip-horiz)))

(defn move [pos dir dist]
  (add pos (mult dir dist)))

(defn manatthan-dist [p]
  (->> p
       (map abs)
       (reduce +)))
