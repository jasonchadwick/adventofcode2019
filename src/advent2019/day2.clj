(ns advent2019.day2
  (:require [advent2019.util :as util])
  (:require [clojure.java.io])
  (:require [clojure.string]))

(defn make-vec [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (vec (map (fn [s] (Integer/parseInt s)) (clojure.string/split (slurp rdr) #",")))))

(defn run-code-1 [v pos]
  (if (>= pos (count v))
    (throw (Exception. "oh no"))
    (case (nth v pos)
      99 (nth v 0)
      1 (run-code-1 (util/operate + v pos) (+ 4 pos))
      2 (run-code-1 (util/operate * v pos) (+ 4 pos)))))

(defn run-code [v]
  (run-code-1 v 0))

(defn do-1202 [fname]
  (run-code (assoc (make-vec fname) 1 12 2 2)))

;part II
(defn find-vals [v s k target]
  (cond
    (= (run-code v) target) v
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
                    ans (run-code v)]
                (cond (= ans target) y
                      (>= y 99) -1
                      :else (recur (inc y)))))]
      (cond (>= y 0) (+ (* 100 x) y)
            (>= x 99) nil
            :else (recur (inc x))))))

;answers for parts 1 and 2
`(~(do-1202 "resources/day2.txt") ~(test-1-2 (make-vec "resources/day2.txt") 19690720))