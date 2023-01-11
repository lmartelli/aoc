(ns aoc.algo
  (:require
   [clojure.test :refer :all]))

(def ^:dynamic *debug* false)

(defmacro explore
  "Explores a space from a starting position, until a condition is reached.
  Returns a map with :visited (set of all visited positions), :nb-steps (the number of executed
  steps), and :last-visited (the positions visited during the last step)

  `stop?` is a form that has access to last-visited, visited and nb-steps (Exploration always stops anyway when there is nothing left to explore)
  `neighbours` is a function of one argument that lists neighbours of a given position
  `neighbour-allowed?` is a form that has access to `pos` and `neighbour-pos` that
  used to filter neighbours that have not been alreaady visited."
  [&{:keys [start neighbours neighbour-allowed? stop?] :or {neighbour-allowed? true
                                                            stop? '(empty? last-visited)}}]
  `(loop [~'last-visited #{~start} ;; we need a set to test reach of end position efficiently
          ~'visited #{~start}
          ~'nb-steps 0]
     (when *debug* (println (format "step=%3d #last-visited=%5d #visited=%6d" ~'nb-steps (count ~'last-visited) (count ~'visited))))
     (if-let [~'stop (or ~stop? (empty? ~'last-visited))]
       {:visited ~'visited :last-visited ~'last-visited :nb-steps ~'nb-steps :stop ~'stop}
       (let [~'new-positions (mapcat (fn [~'pos] (->> (~neighbours ~'pos)
                                                      (filter (fn [~'neighbour-pos]
                                                                (and (not (~'visited ~'neighbour-pos))
                                                                     ~neighbour-allowed?)))))
                                     ~'last-visited)]
         (recur (into #{} ~'new-positions)
                (apply conj ~'visited ~'new-positions)
                (inc ~'nb-steps))))))

(defn find-cycle [seq]
  (loop [visited {}
         pos 0
         [current & more] seq]
    (if-let [start (visited current)]
      {:start-pos start
       :start-value current
       :length (- pos start)}
      (recur (assoc visited current pos)
             (inc pos)
             more))))

(defn find-cycle-key [key seq]
  (loop [visited-keys {}
         visited-values []
         pos 0
         [current & more] seq]
    (let [k (key current)]
      (if-let [start (visited-keys k)]
        {:start-pos start
         :start-value (visited-values start)
         :repeat-pos pos
         :repeat-value current
         :length (- pos start)
         :values visited-values}
        (recur (assoc visited-keys k pos)
               (conj visited-values current)
               (inc pos)
               more)))))

(defn high-low
  "Find [`low` `high`] such that start ≤ low < high and `(pred (f low))` is false and `(pred (f high))`is true
  See also [[find-min-parameter]]"
  [start f pred]
  (loop [cur start]
    (let [val (f cur)]
      (if (pred val)
        [(/ cur 2) cur val]
        (recur (* cur 2))))))

(defn find-min-parameter
  "Find minimum value `x` greater than `start` such that `(pred (f x))` returns true.
  `(pred (f x))` must be monotonous on [start, +∞["
  [start f pred]
  (loop [[low high high-val] (high-low start f pred)]
    (if (<= (- high low) 1)
      [high high-val]
      (let [center (quot (+ high low) 2)
            center-val (f center)]
        (if (pred center-val)
          (recur [low center center-val])
          (recur [center high high-val]))))))

;; Tests

(deftest find-min-test
  (are [start] (= [36 1296] (find-min-parameter start #(* % %) #(>= % 1234)))
    1 2 3 4 5 6 7))
