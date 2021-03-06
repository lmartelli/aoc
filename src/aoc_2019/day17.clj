(ns aoc-2019.day17
  (:require
   [aoc.core :refer :all]
   [aoc-2019.intcode :refer [run run-prog]]
   [clojure.core.async :as async :refer [>!! <!! poll! close! chan thread]]
   [clojure.string :refer [join]]
   [clojure.test :refer :all]))

(puzzle-input-int-array)

;; part 1

(defn output-reader [scaffolds]
  (let [x (atom 0)
        y (atom 0)]
    (fn [val]
      (let [c (char val)]
        (case c
          \newline (do (swap! y inc)
                       (reset! x 0))
          (do (when-not (= \. c) (swap! scaffolds assoc [@x @y] c))
              (swap! x inc)))))))

(defn print-map [prog]
  (run-prog prog #() #(print (char %)))
  nil)

(defn make-map [prog]
  (let [m (atom {})
        r (output-reader m)]
    (run-prog prog #() r)
    @m))

(defn intersection? [m [x y]]
  (every? #{\#} (map m [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(defn find-intersections [m]
  (for [[coord c] m
        :when (and (= c \#)
                   (intersection? m coord))]
    coord))

(defn alignement-parameter [intersections]
  (reduce + (map #(apply * %) intersections)))

(defpart part1 [input]
  (-> (make-map input)
      find-intersections
      alignement-parameter))

;; part 2

(def directions {\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]})

(defn scaffold? [m pos] (= \# (m pos)))

(defn advance [m pos dir]
  (loop [dist 1]
    (if (not (scaffold? m (add pos (mult dir (inc dist)))))
      dist
      (recur (inc dist)))))

(defn find-direction [m pos dir]
  (cond
    (scaffold? m (add pos dir)) (advance m pos dir)
    (scaffold? m (add pos (rotate-left dir))) "L"
    (scaffold? m (add pos (rotate-right dir))) "R"
    :else nil))

(defn make-path [m pos dir]
  (->>
   (loop [pos pos
          dir dir
          path []]
     (let [next (find-direction m pos dir)]
       (if (nil? next)
         path
         (case next
           "L" (recur pos (rotate-left dir) (conj path next))
           "R" (recur pos (rotate-right dir) (conj path next))
           (recur (add pos (mult dir next)) dir (conj path next))))))
   (partition 2)
   (mapv #(join "," %))))

(defn starts-with? [v v1]
  (let [l (count v1)]
    (and (<= l (count v))
         (= (subvec v 0 l) v1))))

(defn starts-with-any? 
  "Returns a used-sub-path-key or nil."
  [path sub-paths]
  (some (fn [[name sub-path]] (when (starts-with? path sub-path) name))
        sub-paths))

(defn use-sub-paths
  "Returns a [remaining-path [used-sub-path-keys]] or nil."
  [path sub-paths]
  (loop [remaining-path path, used-sub-path-keys []]
    (if (empty? remaining-path)
      [remaining-path used-sub-path-keys]
      (if-let [used (starts-with-any? remaining-path sub-paths)]
          (recur (subvec remaining-path (count (sub-paths used))) (conj used-sub-path-keys used))
          [remaining-path used-sub-path-keys]))))

(defn length-valid? [c]
  (<= (+ (reduce + (map count c))
         (dec (count c)))
     20))

(defn make-sub-paths
  "Generate valid sub-paths starting at path"
  [path]
  (for [len (range 2 (inc (count path)))
        :let [sub-path (subvec path 0 len)]
        :while (length-valid? sub-path)]
    sub-path))

(defn compress- [remaining-path sub-paths main sub-path-names]
  ;; (println "names =" sub-path-names)
  ;; (println "path =" remaining-path)
  ;; (println "sub-paths =" sub-paths)
  ;; (println "main =" main)
  (cond
    (empty? remaining-path) [main sub-paths]
    (not (length-valid? main)) nil
    (empty? sub-path-names) nil
    :else
    (some
     (fn [sub-path]
       (let [sub-paths (assoc sub-paths (first sub-path-names) sub-path)
             [remaining-path used-sub-paths] (use-sub-paths remaining-path sub-paths)]
         (compress- remaining-path
                    sub-paths
                    (concat main used-sub-paths)
                    (rest sub-path-names))))
     (make-sub-paths remaining-path))))

(defn compress [path sub-path-names]
  (compress- path {} [] sub-path-names))

(defn char-range [start count]
  (take count (map #(str (char (+ % (int \A)))) (range))))

(defn join-lines [lines]
  (apply str (map #(str % \newline) lines)))

(defpart part2 [input]
  (let [m (make-map input)
        [pos dir] (first (filter (fn [[k v]] (#{\^ \v \> \<} v)) m))
        dir (directions dir)
        sub-path-names (char-range \A 3)]
    (let [path (make-path m pos dir)
          [main sub-paths] (compress path sub-path-names)
          ascii-data (join-lines
                      (-> []
                          (conj (join "," main))
                          (into (map #(join "," (sub-paths %)) sub-path-names))
                          (conj "n")))]
      (let [prog-in (chan 1)
            prog-out (atom 0)]
        (thread (run! #(>!! prog-in (int %)) ascii-data))
        (run-prog
         (assoc input 0 2)
         #(<!! prog-in)
         #(reset! prog-out %))
        @prog-out))))

;; tests

(deftest output-reader-test
  (let [m (atom {})
        reader (output-reader m)]
    (doall (map #(reader %) (str ".#.\n"
                                 ".##\n"
                                 "..#\n"
                                 ">##")))
    (is (= {[1 0] \#
            [1 1] \# [2 1] \#
            [2 2] \#
            [0 3] \> [1 3] \# [2 3] \#}
           @m))))

(deftest starts-with?-test
  (are [v prefix] (starts-with? v prefix)
    [0 1 2 3 4] [0]
    [0 1 2 3 4] [0 1]
    [0 1 2 3 4] [0 1 2]
    [0 1 2 3 4] [0 1 2 3]
    [0 1 2 3 4] [0 1 2 3 4])
  (are [v prefix] (not (starts-with? v prefix))
    [0 1 2 3 4] [9]
    [0 1 2 3 4] [0 1 9]
    [0 1 2 3 4] [0 1 2 9]
    [0 1 2 3 4] [0 1 2 3 9]
    [0 1 2 3 4] [0 1 2 3 4 9]))

(deftest make-sub-paths-test
  (are [paths lengths sub-paths] (= sub-paths (make-sub-paths paths lengths))
    [0 1 2 3 4 5] [] []
    [0 1 2 3 4 5] [1] [[0]]
    [0 1 2 3 4 5] [2] [[0 1]]
    [0 1 2 3 4 5] [2 2] [[0 1] [2 3]]
    [0 1 2 3 4 5] [2 2 1] [[0 1] [2 3] [4]]))

(deftest length-valid?-test
  (are [path] (length-valid? path)
    []
    ["A"]
    [(apply str (repeat 20 "X"))]))

(deftest starts-with-any?-test
  (are [path sub-paths expected] (= expected (starts-with-any? path sub-paths))
    [0 1 2 3] {:a [1] :b [0 1]} :b
    [0 1 2 3] {:a [0] :b [1 2 3]} :a
    [0 1 2 3] {:a [1] :b [2 3]} nil))

(deftest use-sub-paths-test
  (are [path sub-paths expected] (= expected (use-sub-paths path sub-paths))
    [0 1 2 3] {:a [1] :b [0 1]} [[2 3] [:b]]
    [0 1 2 3] {:a [0] :b [1 2 3]} [[] [:a :b]]
    [0 1 2 3] {:a [1] :b [2 3]} [[0 1 2 3] []]
    [0 1 2 3 0 1 2 3 4 5] {:a [0 1] :b [2 3]} [[4 5] [:a :b :a :b]]
    [0 1 2 3 0 1 2 3 4 5] {:a [0 1 2 3] :b [2 3]} [[4 5] [:a :a]]))
