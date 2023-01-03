(ns aoc-2016.day17
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.md5 :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn pos-to-index [[x y]]
  (+ x (* y 4)))

(defn index-to-pos [i]
  [(mod i 4) (quot i 4)])

(def directions
  (->> (for [i (range (* 4 4))
             :let [pos (index-to-pos i)]]
          (->> (s2/direct-neighbours pos [0 3] [0 3])
               (map #({[1 0] \R, [-1 0] \L, [0 1] \D, [0 -1] \U} (s2/- % pos)))))
       (into [])))

(defn move [i m]
  (case m
    \R (inc i)
    \L (dec i)
    \U (- i 4)
    \D (+ i 4)))

(defn opened-doors [path]
  (->> (md5 path)
       (bytes-to-quadbits)
       (take 4)
       (keep-indexed (fn [i hash] (when (> hash 10) (get "UDLR" i))))
       set))

(defn neighbours [[i path]]
  (->> (filter (opened-doors path) (directions i))
       (map #(vector (move i %) (str path %)))))

(defn valid-path? [[i _]]
  (= 15 i))

(defn find-path [passcode]
  (loop [paths [[0 passcode]]]
    (if-let [[i path-to-vault] (find-first valid-path? paths)]
      (subs path-to-vault(count passcode))
      (if (empty? paths)
        nil
        (recur (mapcat neighbours paths))))))

(defpart part1 [passcode]
  (find-path passcode))

;; part 2

(defn longest-path-length [passcode]
  (loop [paths [[0 passcode]]
         longest 0]
    #_(println "nb-paths" (count paths))
    (if (empty? paths)
      longest
      (let [{valid-paths true, invalid-paths false} (group-by valid-path? paths)]
        (recur
          (mapcat neighbours invalid-paths)
          (if-let [[i path-to-vault] (first valid-paths)]
            (- (count path-to-vault) (count passcode))
            longest))))))

(defpart part2 [passcode]
  (longest-path-length passcode))

;; tests

(deftest find-path-test
  (are [passcode expected] (= expected (find-path passcode))
    "ihgpwlah" "DDRRRD"
    "kglvqrro" "DDUDRLRRUDRD"
    "ulqzkmiv" "DRURDRUDDLLDLUURRDULRLDUUDDDRR"))

(deftest longest-path-length-test
  (are [passcode expected] (= expected (longest-path-length passcode))
    "ihgpwlah" 370
    "kglvqrro" 492
    "ulqzkmiv" 830))
