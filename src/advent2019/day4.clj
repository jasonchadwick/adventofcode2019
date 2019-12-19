(ns advent2019.core
  (:require [clojure.set]))

(defn mem [elem coll]
  (cond (empty? coll) false
        (= elem (first coll)) true
        :else (recur elem (rest coll))))

(defn first-sat [p coll]
  (cond (empty? coll) nil
        (p (first coll)) `(~(first coll) ~(rest coll))
        :else (recur p (rest coll))))

(defn lazy-filter [p coll]
  (lazy-seq (let [next (first-sat p coll)]
              (if (nil? next) '() 
                  (cons (first next) 
                        (lazy-filter p (second next)))))))

(defn digit [x n]
  (mod (quot x (Math/pow 10 n)) 10))

(defn apply-adjacent [f coll acc]
  (if (< (count coll) 2) acc
      (recur f (rest coll) (cons (f (first coll) (second coll)) acc))))

(defn only-equal [x n1 n2]
  (and (== (digit x n1) (digit x n2))
       (not (mem (digit x n1) (map (partial digit x) (seq (clojure.set/difference (set (range 6)) #{n1 n2})))))))

(defn get-pwords [min max part2]
  (let [p (fn [x] (and (not (== 0 (digit x 5)))
                       (== 0 (digit x 6))
                       (<= min x) (<= x max)
                       (if part2 ;part2 uses only-equal, part1 just uses equal
                         (some identity
                               (apply-adjacent (partial only-equal x) (range 6) nil))
                         (some identity 
                               (apply-adjacent
                                (fn [a b] (== (digit x a) (digit x b))) (range 6) nil)))
                       (every? identity
                               (apply-adjacent
                                (fn [a b] (>= (digit x a) (digit x b))) (range 6) nil))))]
    (count (lazy-filter p (range min (inc max))))))

;answers for parts 1 and 2
`(~(get-pwords 165432 707912 false) ~(get-pwords 165432 707912 true))