(ns advent2019.day4
  (:require [advent2019.util :as util])
  (:require [clojure.set]))

(defn apply-adjacent [f coll acc]
  (if (< (count coll) 2) acc
      (recur f (rest coll) (cons (f (first coll) (second coll)) acc))))

(defn only-equal [x n1 n2]
  (and (== (util/digit x n1) (util/digit x n2))
       (not (util/mem (util/digit x n1) 
                      (map (partial util/digit x) 
                           (seq (clojure.set/difference (set (range 6)) 
                                                        #{n1 n2})))))))

(defn get-pwords [min max part2]
  (let [p (fn [x] (and (not (== 0 (util/digit x 5)))
                       (== 0 (util/digit x 6))
                       (<= min x) (<= x max)
                       (if part2 ;part2 uses only-equal, part1 just uses equal
                         (some identity
                               (apply-adjacent (partial only-equal x) (range 6) nil))
                         (some identity 
                               (apply-adjacent
                                (fn [a b] (== (util/digit x a) (util/digit x b))) (range 6) nil)))
                       (every? identity
                               (apply-adjacent
                                (fn [a b] (>= (util/digit x a) (util/digit x b))) (range 6) nil))))]
    (count (util/lazy-filter p (range min (inc max))))))

;answers for parts 1 and 2
`(~(get-pwords 165432 707912 false) ~(get-pwords 165432 707912 true))