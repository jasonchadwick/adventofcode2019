(ns advent2019.day3
  (:require [advent2019.util :as util])
  (:require [clojure.java.io])
  (:require [clojure.string])
  (:require [clojure.set]))

(defn get-intersections [coll1 coll2]
  (clojure.set/intersection (set coll1) (set coll2)))

(defn min-intersection [ints]
  (apply min (map (fn [pt] (+ (Math/abs (first pt)) (Math/abs (second pt))))
                  ints)))

(defn do-move [dir moves-left cur coll pts]
  (cond 
    (not (= 0 moves-left))
    (let [nextpt (case dir
                   "U" `(~(first cur) ~(inc (second cur)))
                   "R" `(~(inc (first cur)) ~(second cur))
                   "D" `(~(first cur) ~(dec (second cur)))
                   `(~(dec (first cur)) ~(second cur)))]
      (recur dir (dec moves-left) nextpt coll (cons nextpt pts)))
    
    (seq coll)
    (let [m (first coll)
          newdir (subs m 0 1)
          newmoves (Integer/parseInt (subs m 1))]
      (recur newdir newmoves cur (rest coll) pts))
    
    :else pts))

(defn parse-moves [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (map (fn [str] (clojure.string/split (clojure.string/trim-newline str) #","))
         (clojure.string/split (slurp rdr) #"\n"))))

(defn min-move [fname]
  (let [moves (parse-moves fname)]
    (min-intersection (get-intersections (do-move nil 0 '(0 0) (first moves) nil)
                                         (do-move nil 0 '(0 0) (second moves) nil)))))

;part II
(defn steps-to-pt [ptlist acc pt]
  (if (= pt (first ptlist))
    acc
    (recur (rest ptlist) (inc acc) pt)))

(defn min-steps [pts1 pts2 ints]
  (let [steps1 (map (partial steps-to-pt pts1 1) ints)
        steps2 (map (partial steps-to-pt pts2 1) ints)]
    (apply min (util/zip-map + steps1 steps2 nil))))

(defn get-min-steps [fname]
  (let [moves (parse-moves fname)
        pts1 (reverse (do-move nil 0 '(0 0) (first moves) nil))
        pts2 (reverse (do-move nil 0 '(0 0) (second moves) nil))]
    (min-steps pts1 pts2 (get-intersections pts1 pts2))))

;answers for parts 1 and 2
`(~(min-move "resources/day3.txt") ~(get-min-steps "resources/day3.txt"))