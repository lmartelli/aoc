(ns aoc.vector)

(defn add "Vector addition"
  [a & rest]
  (apply mapv + a rest))

(defn sub "Vector subtraction"
  [a & rest]
  (apply mapv - a rest))

(defn mult "Vector multiplication by a number: v × n"
  [v n]
  (mapv #(* % n) v))

(defn div "Vector division by a number: v × 1/n"
  [v n]
  (mapv #(/ % n) v))

(defn center [v]
  (mult v (/ 1 2)))

(defn prod "Scalar product of 2 vectors" [u v]
  (reduce + (map * u v)))

(defn norm [v]
  (sqrt (reduce + (map square v))))

(defn cos [v u]
  (/ (prod u v) (* (norm u) (norm v))))

(defn remove-index [array index]
  (vec
   (concat
    (subvec array 0 index)
    (subvec array (inc index)))))

(defn shift-right
  ([v] (shift-right v 1))
  ([v n]
   (let [size (count v)]
     (if (zero? size)
       v
       (let [pos (- size (mod n size))]
         (vec (concat (subvec v pos) (subvec v 0 pos))))))))

(defn shift-left
  ([v] (shift-left v 1))
  ([v n] (shift-right v (- n))))

(defn remove-index [v index]
  (vec
   (concat
    (subvec v 0 index)
    (subvec v (inc index)))))

(defn insert-at [v pos value]
  (-> (subvec v 0 pos)
      (conj value)
      (into (subvec v pos))))

(defn swap [v i1 i2]
  (assoc v
         i1 (v i2)
         i2 (v i1)))

(defn index-of [v value]
  (let [index (.indexOf v value)]
    (if (= -1 index)
      nil
      index)))
