(ns advent2019.day7
  (:require [advent2019.intcode :as intcode])
  (:require [clojure.set :as set]))

(defn try-settings [settings input]
  (if (empty? settings) input
      (recur (rest settings) 
             (intcode/run-from-file "resources/day7.txt"
                               (first settings) input))))

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