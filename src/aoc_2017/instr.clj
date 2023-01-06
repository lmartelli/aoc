(ns aoc-2017.instr
  (:require
   [aoc.cpu :refer :all]
   [clojure.test :refer :all]))

(def basic-instr-set
  {:set (instr [reg val] (set-reg reg val))
   :add (instr [reg arg] (update-reg reg + arg))
   :sub (instr [reg arg] (update-reg reg - arg))
   :mul (instr [reg arg] (update-reg reg * arg))
   :mod (instr [reg arg] (update-reg reg mod arg))
   :jnz (instr [t offset] (jump-if offset #(not= 0 %) t))
   :jgz (instr [t offset] (jump-if offset pos? t))})

(deftest basic-instruction-set-test
  (testing "Setting registers"
    (are [instr registers expected] (= expected (exec-instr instr registers basic-instr-set))
      [:set :a 42] {} {:a 42}
      [:set :a :b] {:b 42} {:a 42 :b 42}))
  (testing "Arithmetics"
    (are [instr registers expected] (= expected (exec-instr instr registers basic-instr-set))
      [:add :a 3] {:a 5} {:a 8}
      [:add :a :b] {:a 5 :b 4} {:a 9 :b 4}
      [:add :a :b] {:a 5 :b -1} {:a 4 :b -1}
      [:sub :a 3] {:a 5} {:a 2}
      [:sub :a :b] {:a 5 :b 4} {:a 1 :b 4}
      [:sub :a :b] {:a 5 :b -1} {:a 6 :b -1}
      [:mul :a 3] {:a 5} {:a 15}
      [:mul :a :b] {:b 5} {:a 0 :b 5}
      [:mul :a :b] {:a 3 :b 5} {:a 15 :b 5}
      [:mod :a 3] {:a 0} {:a 0}
      [:mod :a 3] {:a 1} {:a 1}
      [:mod :a 3] {:a 2} {:a 2}
      [:mod :a 3] {:a 3} {:a 0}
      [:mod :a :b] {:a 15 :b 9} {:a 6 :b 9}
    ))
  (testing "Jumps"
    (are [instr registers expected] (= expected (exec-instr instr registers basic-instr-set))
      [:jgz 0 3] {} 1
      [:jgz -1 3] {} 1
      [:jgz 1 3] {} 3
      [:jgz 1 :a] {:a -2} -2
      [:jgz :t :a] {:a -2 :t 0} 1
      [:jgz :t :a] {:a -2 :t -1} 1
      [:jgz :t :a] {:a -2 :t 1} -2)))
