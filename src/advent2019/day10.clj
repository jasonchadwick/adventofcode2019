(ns advent2019.day10
  (:require [advent2019.util :as util])
  (:require [clojure.java.io]))

(defn get-rocks [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (let [board (line-seq rdr)]
      (filter #(= \# (nth (nth board (first %)) (second %)))
              (util/prod (range (count board))
                         (range (count (first board))))))))

; returns list where keys are slopes, and values are lists of points 
; with that slope (slope WRT pt)
(defn get-slope-map [fname pt]
  (into
   (sorted-map)
   (let [grid (get-rocks fname)]
     (util/mk-assoc-list
      (map #(let [dy (- (first pt) (first %))
                  dx (- (second pt) (second %))]
              (mod (- (Math/atan2 dy dx) (* 1/2 Math/PI)) (* 2 Math/PI)))
           grid)
      grid
      (fn [x] (sort-by #(util/dist (first %) (second %)) x))))))

(defn most-visible [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (let [board (line-seq rdr)]
      (reduce #(if (>= (first %1) (first %2)) %1 %2)
              (for [row (range (count board))
                    col (range (count (first board)))]
                `(~(if (= \# (nth (nth board row) col))
                     (count (get-slope-map fname `(~row ~col)))
                     0)
                  ~row
                  ~col))))))

; part 2

; retrieves next non-nil item from a list of lists
; returns found item consed with its index and a new coll without the item.
; loops infinitely if coll is all nil's.
(defn get-next [n coll]
  (let [sublist (nth coll n)]
    (if (nil? sublist)
      (recur (mod (inc n) (count coll)) coll)
      (let [item (first sublist)]
        `(~item ~n ~(assoc coll n (rest sublist)))))))

(defn remove-n [n s-map]
  (loop [counter 1
         idx 0
         vs (vec (vals s-map))]
    (if (= n counter) (first (get-next idx vs))
        (let [x (get-next idx vs)]
          (recur 
           (inc counter) 
           (mod (inc (second x))
                (dec (count vs)))
           (last x))))))

(defn get-pt2-ans [fname pt n]
  (let [point (remove-n n (get-slope-map fname pt))]
    (+ (* 100 (second point)) (first point))))

`(~(first (most-visible "resources/day10.txt"))
  ~(get-pt2-ans "resources/day10.txt" '(28 26) 200)) ;best is (r,c) = (28,26)