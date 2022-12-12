(ns aoc.algo)

(defn count-min-steps
  "Counts minimum number of steps from a start position until stop condition,
  using :directions steps, constrained by :allow-neighbour?

  `start` is the start position
  `next-positions` is a function of that returns the next possible position from a given position
  `stop?` is a predicate that tells when to stop exploring. It must accept a set of positions."
  [&{:keys [start next-positions stop?]}]
  (loop [last-visited #{start}
         visited #{start}
         nb-steps 0]
    (if (stop? last-visited)
      nb-steps
      (let [new-positions (->> (mapcat next-positions last-visited) (filter #(not (visited %))))]
        (recur (into #{} new-positions)
               (apply conj visited new-positions)
               (inc nb-steps))))))
