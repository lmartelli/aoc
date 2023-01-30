(ns aoc.algo
  (:require
   [aoc.core :refer [remove-keys multimap multimap-invert map-vals with-default]]
   [clojure.set :refer [map-invert intersection]]
   [clojure.data.priority-map :refer [priority-map]]))

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

(defn find-cycle
  "Finds a cycle in an iterated sequence.

  Returns a map with :start-pos :start-value and :length."
  [seq]
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

(defn nth-cycling-iteration
  "Gets the `n` element in an iterated sequence that cycles"
  [seq n]
  (loop [visited {}
         indexed []
         pos 0
         [current & more] seq]
    (if-let [start (visited current)]
      (indexed (+ start
                  (-> n
                      (- start)
                      (mod (- pos start)))))
      (recur (assoc visited current pos)
             (conj indexed current)
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

(defn- build-path [dest visited]
  (loop [cur dest
         path (list dest)]
    (if-let [prev (visited cur)]
      (recur prev (conj path prev))
      path)))

(defn- build-path [dest predecessor]
  (loop [cur dest
         path nil]
    (if (nil? cur)
      path
      (recur (predecessor cur) (conj path cur)))))

(defn- min-pos [a b]
  (cond
    (nil? a) b
    (neg? (compare a b)) a
    :else b))

(defn- max-pos [a b]
  (cond
    (nil? a) b
    (pos? (compare a b)) b
    :else a))

(defn bfs-path
  "Gets path from `start` to closest of `destinations`.
  If 2 destination are at the same distance, the first from the sequence is chosen.
  If none of the destinations are reachable, returns nil.
  For a given destination, ties in steps are broken using `choose-position`:
  it can be `:first`, `:last` `:min` or `:max`."
  [&{:keys [start neighbours destinations pred-choice] :or {pred-choice :any}}]
  (let [choose-pred (case pred-choice
                      :any #(or %1 %2)
                      :min  min-pos
                      :max  max-pos)]
    (loop [last-visited #{start}
           predecessors {start nil}]
      (if-let [dest (some last-visited destinations)]
        (build-path dest predecessors)
        (if (empty? last-visited)
          nil
          (recur
            (set (remove #(contains? predecessors %) (mapcat neighbours last-visited)))
            (reduce
              (fn [new-predecessors cur-pos]
                (reduce
                  (fn [new-predecessors next-pos]
                    (update new-predecessors next-pos choose-pred cur-pos))
                  new-predecessors
                  (remove #(contains? predecessors %) (neighbours cur-pos))))
              predecessors
              last-visited)))))))

(defn bfs-max-steps
  "Finds the number of steps to reach farthest node in a graph.
  See also [[tree-depth]]"
  [&{:keys [start neighbours]}]
  (loop [last-visited #{start}
         nb-steps 0]
    (if (empty? last-visited)
      (dec nb-steps)
      (recur
        (set (remove last-visited (mapcat neighbours last-visited)))
        (inc nb-steps)))))

(defn tree-depth
  "`children` should be a map node -> childen
  `root` is the node from which to compute the depth.
  See also [[bfs-max-steps]]"
  [&{:keys [root children]}]
  (loop [last-visited [root]
         depth 0]
    (if (empty? last-visited)
      (dec depth)
      (recur
        (mapcat children last-visited)
        (inc depth)))))

(defn make-adjacency-graph
  ([rows] (make-adjacency-graph rows nil))
  ([rows nil-vertice]
   (let [nil-row (repeat nil-vertice)
         up    (map (fn [cur-row prev-row]
                      (map (fn [cur prev] prev) cur-row (concat prev-row (repeat nil-vertice))))
                    rows (cons nil-row rows))
         down  (map (fn [cur-row next-row]
                      (map (fn [cur next] next) cur-row (concat next-row (repeat nil-vertice))))
                    rows (concat (rest rows) [nil-row]))
         left  (map (fn [cur-row]
                      (cons nil-vertice (butlast cur-row)))
                    rows)
         right (map (fn [cur-row]
                      (concat (rest cur-row) [nil-vertice]))
                    rows)]
     (->> (mapcat (fn [row & neighbour-rows]
                    (apply map (fn [vertice & neighbours]
                                 [vertice (remove #(= nil-vertice %) neighbours)])
                           row
                           neighbour-rows))
                  rows up left right down)
          (remove (fn [[vertice neighbours]] (or (empty? neighbours) (= nil-vertice vertice))))
          (into {})))))

(defn adjacency-graph
  "Builds a graph node→neighbours.
  `nodes` is a set of all nodes
  `neighbours` is a function that returns potential neighbours of a node."
  [nodes neighbours]
  (reduce
    (fn [G node]
      (if-let [ns (->> (neighbours node)
                       (filter nodes)
                       not-empty)]
        (assoc G node ns)
        G)
      )
    {}
    nodes))

(defn make-weigthed-graph [edges]
  (reduce
    (fn [W [{:keys [from to]} weight]]
      (assoc-in W [from to] weight))
    {}
    edges))

(defn weighted-graph
  "Builds a weighted graph for a subset of the `nodes` of a non-weighted graph `G`.
  The weights are the minimum numbers of hops"
  [G nodes]
  (let [node? (set nodes)]
    (loop [edges (into {}
                       (mapcat
                         (fn [node]
                           (map #(vector {:from node :to %} 1) (G node)))
                         nodes))
           visited (into #{} (map #(hash-map :from % :to %) nodes))]
      (let [to-remove (->> edges
                           (remove (comp node? :to key)))]
        (if (empty? to-remove)
          (make-weigthed-graph edges)
          (recur
            (reduce
              (fn [edges [{:keys [from to] :as removed-edge} weight]]
                (reduce
                  (fn [edges new-neighbour]
                    (assoc edges {:from from :to new-neighbour} (inc weight)))
                  (dissoc edges removed-edge)
                  (->> (G to)
                       (remove (fn [node]
                                 (visited {:from from :to node}))))))
              edges
              to-remove)
            (into visited (keys to-remove))))))))

(defn- unique-mappings
  "Returns s submap of the multimap, for which keys have a single value"
  [multimap]
  (persistent!
    (reduce
      (fn [m [k [v & more]]]
        (if (nil? more)
          (assoc! m k v)
          m))
      (transient {})
      multimap)))

(defn- filter-mappings
  [multimap resolved-keys resolved-vals]
  "Remove values present in `resolved-val` and keys present in `resolved-keys`"
  (as-> multimap $
      (update-vals $ #(if (> (count %) 1) (remove resolved-vals %) %))
      (remove-keys resolved-keys $)))

(defn resolve-bijection
  "Given `mm`, a multimap k → vs of possible values for `(m x)`,
  find a bijection `m` such that `(m k)` is in `(mm k)`.
  Returns nil if no such bijection can be deduced."
  [mm]
  (loop [bijection {}
         k->vs mm
         v->ks (multimap-invert mm)]
    (if (= (count mm) (count bijection))
      bijection
      (let [new-unique-keys (merge
                              (unique-mappings k->vs)
                             (map-invert (unique-mappings v->ks)))]
        (if (empty? new-unique-keys)
          nil
          (let [bijection (merge bijection new-unique-keys)
                v->k (map-invert bijection)]
        (recur
          bijection
          (filter-mappings k->vs bijection v->k)
          (filter-mappings v->ks v->k bijection))))))))

(defn resolve-bijection-from-samples
  "`samples` is a sequence of [key [val1 val2 ...]]."
  [samples]
  (->> samples
       multimap
       (map-vals (comp #(reduce intersection %) #(map set %)))
       resolve-bijection))

(defn a* [&{:keys [start goal neighbours heuristic]}]
  (loop [{:keys [open-set came-from g-score] :as state}
         {:open-set (priority-map start (heuristic start))
          :came-from {}
          :g-score (with-default {start 0} ##Inf)}]
    (let [[current] (peek open-set)]
      (if (= goal current)
        {:cost (g-score current)
         :path (build-path goal came-from)}
        (let [current-score (g-score current)
              neighbours-score (->> (neighbours current)
                                    (map (fn [[neighbour weight]]
                                           [neighbour (+ current-score weight)]))
                                    (filter (fn [[neighbour score]]
                                              (< score (g-score neighbour)))))]
          (recur
            {:open-set (into (dissoc open-set current)
                             (map (fn [[neighbour score]] [neighbour (+ score (heuristic neighbour))]) neighbours-score))
             :g-score (into g-score neighbours-score)
             :came-from (into came-from (map #(vector % current) (map first neighbours-score)))}))))))
