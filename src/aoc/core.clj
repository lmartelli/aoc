(ns aoc.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.math.combinatorics :as combin]
   [clj-http.client :as http]
   [clojure.core.async :as async :refer [poll!]]))

(defn system-get-property [p]
  (System/getProperty p))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (system-get-property "user.home"))
    s))

(def aoc-dir (expand-home "~/.aoc"))

(def session
  (try (slurp (str aoc-dir "/session"))
       (catch java.io.FileNotFoundException e nil)))

(defn replace-chars
  ([s m] (apply str (replace m s)))
  ([s from-chars to-chars]
   (replace-chars s (zipmap from-chars to-chars))))

(defn ns-name [ns]
  (str (clojure.core/ns-name ns)))

(defn parse-aoc-ns-name [ns-name]
  (if-let [[_ year day] (re-matches #"aoc-(\d+)\.day0*(\d+)" ns-name)]
    [year day]
    (throw (Exception.
            (str "Invalid AOC namespace. Must be like \"aoc-<year>.day<day>\": " ns-name)))))

(defn puzzle-input-uri [year day]
  (str "https://adventofcode.com/" year "/day/" day "/input"))

(defn puzzle-input-filename [year day]
  (let [day (str (when (-> day count (= 1)) "0") day)]
    (str aoc-dir "/puzzle-input-"year "-" day ".txt")))

(defn download-puzzle-input [year day]
  (spit (puzzle-input-filename year day)
        (-> (puzzle-input-uri year day)
            (http/get {:cookies {"session" {:path "/" :value session}}})
            :body)))

(defn $puzzle-input-stream [ns]
  (let [[year day] (parse-aoc-ns-name (ns-name ns))
        filename (puzzle-input-filename year day)]
    (when-not (.exists (io/file filename))
      (download-puzzle-input year day))
    (io/reader filename)))

(defn $puzzle-input-string [ns]
  (->> ($puzzle-input-stream ns)
       slurp
       str/split-lines
       (apply str)))

(defmacro puzzle-input-string []
  `(def ~'puzzle-input ($puzzle-input-string *ns*)))

(defn $puzzle-input-lines [ns]
  (->> ($puzzle-input-stream ns)
       io/reader
       line-seq))

(defmacro puzzle-input-parse-stream [f]
  `(def ~'puzzle-input (~f ($puzzle-input-stream *ns*))))

(defmacro puzzle-input-lines
  ([] `(puzzle-input-lines identity))
  ([xf] `(def ~'puzzle-input (~xf ($puzzle-input-lines *ns*)))))

(defmacro puzzle-input-parse-lines [f]
  `(def ~'puzzle-input (mapv ~f ($puzzle-input-lines *ns*))))

(defmacro puzzle-input-split-lines
  ([regex xf]
   `(puzzle-input-parse-lines #(~xf (str/split % ~regex))))
  ([regex]
   `(puzzle-input-split-lines ~regex identity)))

(defn parse-int [s] (Long/parseLong s))

(defn parse-int-array [input]
  (vec
   (map
    #(Long/parseLong %)
    (str/split input #","))))

(defmacro puzzle-input-int-array []
  `(def ~'puzzle-input (parse-int-array ($puzzle-input-string *ns*))))

(defn digit-seq
  "Parses s as a sequence of digits.
  \"123456\" will return (1 2 3 4 5 6)."
  [s]
  (map #(Character/digit % 10) s))

(defmacro puzzle-input-parse [f]
  `(def ~'puzzle-input (~f ($puzzle-input-string *ns*))))

(defn remove-nil [& colls]
  (apply map #(remove nil? %) colls))

(defn add "Vector addition"
  [a & rest]
  (apply mapv + a rest))

(defn sub "Vector subtraction"
  [a & rest]
  (apply mapv - a rest))

(defn mult "Vector multiplication by a number: v × n"
  [v n]
  (mapv #(* % n) v))

(defn rotate-left [[x y]]
  [y (- x)])

(defn rotate-right [[x y]]
  [(- y)  x])

(defn move [pos dir dist]
  (add pos (mult dir dist)))

(defn manatthan-dist [p]
  (->> p
       (map abs)
       (reduce +)))

(defn read-all [channel]
  (loop [v []]
    (if-let [val (poll! channel)]
      (recur (conj v val))
      v))
  )

(defmacro defpart [name args body]
  `(def ~name
     (fn
       ([] (~name ~'puzzle-input))
       (~args ~body))))

(defn array-2d-to-map [rows]
  (into
   {}
   (mapcat
    (fn [row y]
      (mapv (fn [val x] [[x y] val]) row (range)))
    rows (range))))

(defn remove-index [array index]
  (vec
   (concat
    (subvec array 0 index)
    (subvec array (inc index)))))

(defn grep [regex seq]
  (filter #(re-find regex %) seq))

(defn find-first [f seq]
  (first (filter f seq)))

(defn combinations-with-sum
  "Combinations of n positive integers constrained by a sum"
  [n sum]
  (if (= 1 n)
    (list [sum])
    (mapcat
     (fn [x]
       (map #(conj % x) (combinations-with-sum (dec n) (- sum x))))
     (range (inc sum)))))

