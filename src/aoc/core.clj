(ns aoc.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn replace-chars
  ([s m] (apply str (replace m s)))
  ([s from-chars to-chars]
   (replace-chars s (zipmap from-chars to-chars))))

(defn puzzle-input-resource-path [ns]
  (str (replace-chars (str (ns-name ns)) {\- \_, \. \/}) ".txt"))

(defn puzzle-input-stream [ns]
  (io/resource (puzzle-input-resource-path ns)))

(defn puzzle-input-string [ns]
  (->> (puzzle-input-stream ns)
      slurp
      str/split-lines
      (apply str)))

(defn puzzle-input-lines [ns-name]
  (->> (puzzle-input-stream ns-name)
       io/reader
       line-seq))

(defn parse-int [s] (Integer/parseInt s))
