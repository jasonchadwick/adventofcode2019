(ns advent2019.day2
  (:require [advent2019.intcode :as intcode])
  (:require [clojure.java.io])
  (:require [clojure.string]))

(defn do-1202 [fname]
  (intcode/run-code (assoc (intcode/make-vec fname) 1 12 2 2)))

;part II

;CPS function - overflows because of closure's small stack size
(defn find-vals [v s k target]
  (cond
    (= (intcode/run-code v) target) v
    (and (>= (nth v 1) 65) (>= (nth v 2) 77)) (k)
    (>= (nth v 1) 70) (recur
                      (assoc v 2 (inc (nth v 2))) s k target)
    (>= (nth v 2) 77) (recur
                      (assoc v 1 (inc (nth v 1))) s k target)
    :else (recur (assoc v 1 (inc (nth v 1))) s
                 (fn [] (find-vals (assoc v 2 (inc (nth v 2))) s k target)) target)))

(defn test-1-2 [v target]
  (loop [x 0]
    (let [v (assoc v 1 x)
          y (loop [y 0]
              (let [v (assoc v 2 y)
                    ans (intcode/run-code v)]
                (cond (= ans target) y
                      (>= y 99) -1
                      :else (recur (inc y)))))]
      (cond (>= y 0) (+ (* 100 x) y)
            (>= x 99) nil
            :else (recur (inc x))))))

;answers for parts 1 and 2
`(~(do-1202 "resources/day2.txt") ~(test-1-2 (intcode/make-vec "resources/day2.txt") 19690720))