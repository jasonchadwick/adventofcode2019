(ns advent2019.day7
  (:require [advent2019.intcode :as intcode])
  (:require [clojure.set :as set]))

(defn try-settings [settings input]
  (if (empty? settings) input
      (recur (rest settings)
             (intcode/run-to-end "resources/day7.txt"
                                    0 `(~(first settings) ~input)))))

; works for all three examples, why not mine?
(defn try-all []
  (let [nums #{0 1 2 3 4}]
    (apply 
     max
     (for [a nums
           b (set/difference nums #{a})
           c (set/difference nums #{a b})
           d (set/difference nums #{a b c})
           e (set/difference nums #{a b c d})]
       (try-settings `(~a ~b ~c ~d ~e) 0)))))


; part II

(defn try-settings-repeated [settings input amp pos-vec]
  (if (empty? settings) input
      (recur (rest settings)
             (intcode/run-to-end "resources/day7.txt"
                                 0 `(~(first settings) ~input))
             (mod (inc amp) 5)
             pos-vec)))

(defn try-all-repeated []
  (let [nums #{5 6 7 8 9}]
    (apply
     max
     (for [a nums
           b (set/difference nums #{a})
           c (set/difference nums #{a b})
           d (set/difference nums #{a b c})
           e (set/difference nums #{a b c d})]
       (try-settings-repeated [a b c d e] 0 0 [0 0 0 0 0])))))

(try-all)