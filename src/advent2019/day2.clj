(ns advent2019.day2
  (:require [advent2019.intcode :as intcode])
  (:require [clojure.java.io])
  (:require [clojure.string]))

(defn do-1202 [fname]
  (first (intcode/run-code (assoc (intcode/make-vec fname) 1 12 2 2) 0 nil)))

;part II

(defn test-1-2 [v target]
  (loop [x 0]
    (let [v (assoc v 1 x)
          y (loop [y 0]
              (let [v (assoc v 2 y)
                    ans (first (intcode/run-code v 0 nil))]
                (cond (= ans target) y
                      (>= y 99) -1
                      :else (recur (inc y)))))]
      (cond (>= y 0) (+ (* 100 x) y)
            (>= x 99) nil
            :else (recur (inc x))))))

;answers for parts 1 and 2
`(~(do-1202 "resources/day2.txt") ~(test-1-2 (intcode/make-vec "resources/day2.txt") 19690720))