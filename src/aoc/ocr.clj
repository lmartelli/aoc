(ns aoc.ocr
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn apply-str [xs] (apply str xs))

(defn char-columns-to-str-lines [columns]
  (->> (transpose columns)
       (map #(-> (apply str %) str/trimr))))

(defn split-chars [char-width lines]
  (->> (transpose lines)
       (partition char-width char-width (repeat (repeat (count lines) \space)))
       (map char-columns-to-str-lines)))

(defn mk-font [width data]
  {:width width
   :glyphs  (->> (split-chars width data)
                 (map #(vector %2 (char %1)) (iterate inc (int \A)))
                 (into {}))})

(def small-font
  (mk-font
    5
    [" ██  ███   ██  ███  ████ ████  ██  █  █  ███   ██ █  █ █    █  █ █  █  ██  ███   ██  ███   ███ ███  █  █ █   ██  █ █  █ █   █████ "
     "█  █ █  █ █  █ █  █ █    █    █  █ █  █   █     █ █ █  █    ████ ██ █ █  █ █  █ █  █ █  █ █     █   █  █ █   ██  █ █  █ █   █   █ "
     "█  █ ███  █    █  █ ███  ███  █    ████   █     █ ██   █    █  █ █ ██ █  █ █  █ █  █ █  █ █     █   █  █  █ █ █  █  ██   █ █   █  "
     "████ █  █ █    █  █ █    █    █ ██ █  █   █     █ █ █  █    █  █ █  █ █  █ ███  █  █ ███   ██   █   █  █  █ █ █  █ █  █   █   █   "
     "█  █ █  █ █  █ █  █ █    █    █  █ █  █   █  █  █ █ █  █    █  █ █  █ █  █ █    █ █  █ █     █  █   █  █   █  ████ █  █   █  █    "
     "█  █ ███   ██  ███  ████ █     ███ █  █  ███  ██  █  █ ████ █  █ █  █  ██  █     █ █ █  █ ███   █    ██    █  █  █ █  █   █  ████ "]))

(def big-font
  (mk-font
    8
    ["  ██    █████    ████   █████   ██████  ██████   ████   █    █  █████      ███  █    █  █       █     █ █    █   ████   █████    ████   █████    ████  ███████  █    █  █     █ █     █ █    █  █    █  ██████  "
     " █  █   █    █  █    █  █    █  █       █       █    █  █    █    █         █   █   █   █       ██   ██ ██   █  █    █  █    █  █    █  █    █  █    █    █     █    █  █     █ █     █ █    █  █    █       █  "
     "█    █  █    █  █       █    █  █       █       █       █    █    █         █   █  █    █       █ █ █ █ ██   █  █    █  █    █  █    █  █    █  █         █     █    █  █     █ █     █  █  █   █    █       █  "
     "█    █  █    █  █       █    █  █       █       █       █    █    █         █   █ █     █       █  █  █ █ █  █  █    █  █    █  █    █  █    █  █         █     █    █  █     █ █     █  █  █   █    █      █   "
     "█    █  █████   █       █    █  █████   █████   █       ██████    █         █   ██      █       █     █ █ █  █  █    █  █████   █    █  █████    ████     █     █    █   █   █  █     █   ██     █████     █    "
     "██████  █    █  █       █    █  █       █       █  ███  █    █    █         █   ██      █       █     █ █  █ █  █    █  █       █    █  █  █         █    █     █    █   █   █  █     █   ██         █    █     "
     "█    █  █    █  █       █    █  █       █       █    █  █    █    █     █   █   █ █     █       █     █ █  █ █  █    █  █       █    █  █   █        █    █     █    █    █ █   █  █  █  █  █        █   █      "
     "█    █  █    █  █       █    █  █       █       █    █  █    █    █     █   █   █  █    █       █     █ █   ██  █    █  █       █  █ █  █   █        █    █     █    █    █ █   █ █ █ █  █  █        █  █       "
     "█    █  █    █  █    █  █    █  █       █       █   ██  █    █    █     █   █   █   █   █       █     █ █   ██  █    █  █       █   █   █    █  █    █    █     █    █     █    ██   ██ █    █       █  █       "
     "█    █  █████    ████   █████   ██████  █        ███ █  █    █  █████    ███    █    █  ██████  █     █ █    █   ████   █        ███ █  █    █   ████     █      ████      █    █     █ █    █   ████   ██████  "]))

(defn show [lines]
  (run! (comp println str/join) lines)
  lines)

(defn ocr
  ([lines] (ocr lines \space))
  ([lines off-char] (ocr lines off-char small-font))
  ([lines off-char {:keys [glyphs width]}]
    (->> lines
         (map (fn [line] (map #(if-not (#{off-char} %) \█ \space) line)))
         show
         (split-chars width)
         (map #(if-let [c (glyphs %)]
                 c
                 (throw (Exception. (str "Unrecognized character:\n" (str/join "\n" %))))))
         (apply str))))

(defn ocr-big [lines]
  (ocr lines \space big-font))

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
    )
  (are [lines expected](= expected (ocr-big lines))
    [" ####   ######  #    #  #####   #    #    ##    #####   #    #"
     "#    #       #  #   #   #    #  ##   #   #  #   #    #  ##   #"
     "#            #  #  #    #    #  ##   #  #    #  #    #  ##   #"
     "#           #   # #     #    #  # #  #  #    #  #    #  # #  #"
     "#          #    ##      #####   # #  #  #    #  #####   # #  #"
     "#         #     ##      #       #  # #  ######  #  #    #  # #"
     "#        #      # #     #       #  # #  #    #  #   #   #  # #"
     "#       #       #  #    #       #   ##  #    #  #   #   #   ##"
     "#    #  #       #   #   #       #   ##  #    #  #    #  #   ##"
     " ####   ######  #    #  #       #    #  #    #  #    #  #    #"] "CZKPNARN"))
