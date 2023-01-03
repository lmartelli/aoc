(ns aoc.algo)

(def ^:dynamic *debug* false)

(defmacro explore
  "Explores a space from a starting position, until a condition is reached.
  Returns a map with :visited (set of all visited positions), :nb-steps (the number of executed
  steps), and :last-visited (the positions visited during the last step)

  `stop?` is a form that has access to last-visited, visited and nb-steps (by default exploration stops when there is nothing left to explore)
  `neighbours` is a function of one argument that lists neighbours of a given position
  `neighbour-allowed?` is a form that has access to `pos` and `neighbour-pos` that
  used to filter neighbours that have not been alreaady visited."
  [&{:keys [start neighbours neighbour-allowed? stop?] :or {neighbour-allowed? true
                                                            stop? '(empty? last-visited)}}]
  `(loop [~'last-visited #{~start} ;; we need a set to test reach of end position efficiently
          ~'visited #{~start}
          ~'nb-steps 0]
     (when *debug* (println (format "step=%3d #last-visited=%5d #visited=%6d" ~'nb-steps (count ~'last-visited) (count ~'visited))))
     (if ~stop?
       {:visited ~'visited :last-visited ~'last-visited :nb-steps ~'nb-steps}
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
         :repeat-value current
         :length (- pos start)}
        (recur (assoc visited-keys k pos)
               (conj visited-values current)
               (inc pos)
               more)))))
