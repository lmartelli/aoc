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

(defn- build-path [dest visited]
  (loop [cur dest
         path (list dest)]
    (if-let [prev (visited cur)]
      (recur prev (conj path prev))
      path)))

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

;; Tests

(deftest find-min-test
  (are [start] (= [36 1296] (find-min-parameter start #(* % %) #(>= % 1234)))
    1 2 3 4 5 6 7))

(deftest make-adjacency-graph-test
  (testing "Matrix with any hole"
    (are [rows graph] (= graph (make-adjacency-graph rows))
      '[(a)] {}
      '[(a b)] '{a (b), b (a)}
      '[(a b c)] '{a (b), b (a c), c (b)}
      '[(a b)
        (c d)] '{a (b c), b (a d), c (a d), d (b c)}
      '[(a b c)
        (d e f)
        (g h i)] '{a (b d), b (a c e), c (b f), d (a e g), e (b d f h), f (c e i), g (d h), h (e g i), i (f h)}))
  (testing "Ragged rows"
    (are [rows graph] (= graph (make-adjacency-graph rows))
      '[(a b c)
        (d e)
        (g h i)] '{a (b d), b (a c e), c (b), d (a e g), e (b d h), g (d h), h (e g i), i (h)}))
  (testing "Using default nil-vertice"
    (are [rows graph] (= graph (make-adjacency-graph rows))
      '[(a  b  c)
        (d nil f)
        (g  h  i)] '{a (b d), b (a c), c (b f), d (a g), f (c i), g (d h), h (g i), i (f h)}))
  (testing "Using custom nil-vertice"
    (are [rows graph] (= graph (make-adjacency-graph rows '_))
      '[(a b c)
        (d _ f)
        (g h i)] '{a (b d), b (a c), c (b f), d (a g), f (c i), g (d h), h (g i), i (f h)})))

(deftest bfs-path-test
  (let [graph (make-adjacency-graph ["abcdef"
                                     "gh jkl"
                                     "mnop  "
                                     "stuv x"] \space)]

    (testing "Returns nil if no destination is reachable"
      (are [start destinations]
          (nil? (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\z)
        \a '(\x)))

    (testing "Choose 1st destination in the list"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\b \c) '(\a \b)))

    (testing "Single path"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\a) '(\a)
        \a '(\b) '(\a \b)
        \a '(\c) '(\a \b \c)))

    (testing "Use choose-position if multiple directions are available"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph :choose-position :min))
        \a '(\h) '(\a \b \h)))

    (testing "Choose 1st destination in the list"
      (are [start destinations expected]
          (= expected (bfs-path :start start :destinations destinations :neighbours graph))
        \a '(\b \c) '(\a \b)))
    ))
