(ns aoc.algo)

(def ^:dynamic *debug* false)

(defmacro explore
  "Explores a space from a starting position, until a condition is reached.
  Returns a set of all visited positions, the number of executed
  steps, and the positions visited during the last step

  `stop?` is a form that has access to last-visited, visited and nb-steps
  `neighbours` is a function of one argument that lists neighbours of a given position
  `neighbour-allowed?` is a form that has access to `pos` and `neighbour` that
  used to filter neighbours that have not been alreaady visited."
  [&{:keys [start neighbours neighbour-allowed? stop?] :or {neighbour-allowed? true}}]
  `(loop [~'last-visited #{~start} ;; we need a set to test reach of end position efficiently
          ~'visited #{~start}
          ~'nb-steps 0]
     (when *debug* (println (format "step=%3d #last-visited=%5d #visited=%6d" ~'nb-steps (count ~'last-visited) (count ~'visited))))
     (if ~stop?
       {:visited ~'visited :last-visited ~'last-visited :nb-steps ~'nb-steps}
       (let [~'new-positions (->> (mapcat (fn [~'pos] (->> (~neighbours ~'pos)
                                                           (filter (fn [~'neighbour-pos]
                                                                     (and (not (~'visited ~'neighbour-pos))
                                                                          ~neighbour-allowed?)))))
                                          ~'last-visited)
                                  )]
         (recur (into #{} ~'new-positions)
                (apply conj ~'visited ~'new-positions)
                (inc ~'nb-steps)))))
  )