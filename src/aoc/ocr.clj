(ns aoc.ocr
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [join]]
   [clojure.test :refer :all]))

(defn apply-str [xs] (apply str xs))

(defn char-columns-to-str-lines [columns]
  (->> (transpose columns)
       (map #(apply str %))))

(defn split-chars [char-width lines]
  (->> (transpose lines)
       (partition char-width char-width (repeat (repeat (count lines) \space)))
       (map char-columns-to-str-lines)))

(def font
  (->> (split-chars 5
        [" ██  ███   ██  ███  ████ ████  ██  █  █  ███   ██ █  █ █    █  █ █  █  ██  ███   ██  ███   ███ ███  █  █ █   ██  █ █  █ █   █████ "
         "█  █ █  █ █  █ █  █ █    █    █  █ █  █   █     █ █ █  █    ████ ██ █ █  █ █  █ █  █ █  █ █     █   █  █ █   ██  █ █  █ █   █   █ "
         "█  █ ███  █    █  █ ███  ███  █    ████   █     █ ██   █    █  █ █ ██ █  █ █  █ █  █ █  █ █     █   █  █  █ █ █  █  ██   █ █   █  "
         "████ █  █ █    █  █ █    █    █ ██ █  █   █     █ █ █  █    █  █ █  █ █  █ ███  █  █ ███   ██   █   █  █  █ █ █  █ █  █   █   █   "
         "█  █ █  █ █  █ █  █ █    █    █  █ █  █   █  █  █ █ █  █    █  █ █  █ █  █ █    █ █  █ █     █  █   █  █   █  ████ █  █   █  █    "
         "█  █ ███   ██  ███  ████ █     ███ █  █  ███  ██  █  █ ████ █  █ █  █  ██  █     █ █ █  █ ███   █    ██    █  █  █ █  █   █  ████ "])
       (map #(vector %2 (char %1)) (iterate inc (int \A)))
       (into {})
       ))

(defn show [lines]
  (run! (comp println join) lines)
  lines)

(defn ocr
  ([lines] (ocr lines \space))
  ([lines off-char]
   (->> lines
        (map (fn [line] (map #(if-not (#{off-char} %) \█ \space) line)))
        show
        (split-chars 5)
        (map #(if-let [c (font %)]
                c
                (throw (Exception. (str "Unrecognized character:\n" (join "\n" %))))))
        (apply str))))

;; tests

(deftest ocr-test
  (are [lines expected] (= expected (ocr lines))
    [" ██  "
     "█  █ "
     "█  █ "
     "████ "
     "█  █ "
     "█  █ "] "A"

    ["███   ██  █    ███  ███  ████  ██  █  █ "
     "█  █ █  █ █    █  █ █  █    █ █  █ █  █ "
     "█  █ █    █    █  █ ███    █  █  █ █  █ "
     "███  █ ██ █    ███  █  █  █   ████ █  █ "
     "█ █  █  █ █    █ █  █  █ █    █  █ █  █ "
     "█  █  ███ ████ █  █ ███  ████ █  █  ██  "] "RGLRBZAU"
    ))
