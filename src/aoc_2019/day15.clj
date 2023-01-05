(ns aoc-2019.day15
  (:require
   [aoc.core :refer :all]
   [aoc-2019.intcode :refer [run run-prog]]
   [clojure.core.async :as async :refer [>!! <!! poll! close! chan thread]]
   [clojure.set :refer [intersection union difference]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (first (line-seq stream))
       parse-ints
       (into [])))

;; part 1

(def reverse-directions {1 2, 2 1, 3 4, 4 3})

(def moves {1 [0 -1], 2 [0 1], 3 [-1 0], 4 [1 0]})

(def statuses { 0 :wall, 1 :moved, 2 :oxygen })

(defn step-back [in out pos direction]
  (>!! in (reverse-directions direction))
  (when-not (= (<!! out) :moved) (throw (Exception. "Failed to step back!!!")))
  (sub pos (moves direction)))

(defn find-shortest-path [in out direction shortest-path path pos area]
  (if (or (> direction 4) ;; dead-end
          (and (some? shortest-path) (>= (inc (count path)) (count shortest-path))))
    (if (empty? path)
      [area shortest-path]
      (let [prev-dir (peek path)
            prev-pos (step-back in out pos prev-dir)]
        (recur in out (inc prev-dir) shortest-path (pop path) prev-pos area)))
    (let [new-pos (add pos (moves direction))]
      (if (contains? area new-pos)
        (recur in out (inc direction) shortest-path path pos area)
        (let [new-path (conj path direction)]
          (>!! in direction)
          (case (<!! out)
            :wall (recur in out (inc direction) shortest-path  path pos (assoc area new-pos :wall))
            :oxygen (do (step-back in out pos direction)
                        (recur in out (inc direction) new-path path pos area))
            :moved (recur in out 1 shortest-path new-path new-pos (assoc area new-pos :empty))))))))

(defn discover-area [input]
  (let [in (chan 1)
        out (chan 1 (map statuses))]
    (thread (run-prog input #(<!! in) #(>!! out %)))
    (find-shortest-path in out
                        1
                        nil
                        []
                        [0 0]
                        {[0 0] :empty})))

(defpart part1 [input]
  (count (second (discover-area input))))

;; part 2

(defn path-to-coord [path]
  (reduce add [0 0] (map moves path)))

(defn adjacents [vect]
  (mapcat #(vector (update vect % inc) (update vect % dec)) (range (count vect))))

(defn select [area type]
  (into #{} (map first) (filter (fn [[k v]] (= v type)) area)))

(defpart part2 [input]
  (let [[area path] (discover-area input)]
    (loop [time 0
           frontier #{(path-to-coord path)}
           oxygen frontier
           empty (select area :empty)]
      (if (empty? empty)
        time
        (let [expansion (into #{} (mapcat adjacents frontier))]
          (recur
           (inc time)
           (intersection expansion empty)
           (union oxygen expansion)
           (difference empty expansion))))
      )
    )
  )

;; tests
