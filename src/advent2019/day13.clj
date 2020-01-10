(ns advent2019.day13
  (:require [advent2019.intcode :as intcode]))

(defn get-board [fname]
  (loop [x (intcode/run-code fname 0 0 nil true)
         y (apply intcode/run-code x)
         t (apply intcode/run-code y)
         next (apply intcode/run-code t)
         acc-map {}]
    (if (number? next) (assoc acc-map `(~(first (first x)) ~(first (first y))) (first (first t)))
        (let [x1 next
              y1 (apply intcode/run-code x1)
              t1 (apply intcode/run-code y1)
              next1 (apply intcode/run-code t1)]
          (recur x1 y1 t1 next1
                 (assoc acc-map `(~(first (first x)) ~(first (first y))) (first (first t))))))))

(defn num-blocks [fname]
  (let [board (get-board fname)]
    (count (filter #(= 2 %) (vals board)))))

(num-blocks "resources/day13.txt")