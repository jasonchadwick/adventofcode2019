(ns advent2019.legacy
  (:require [advent2019.util :as util])
  (:require [clojure.java.io]))

;code that I've thrown out but don't want to delete because I spent a lot of time on it

;;;;;;;;;;;;;;;;;;;;
; DAY 10
;;;;;;;;;;;;;;;;;;;;

(defn is-blocked-by [board pt deltax deltay [r c]]
  (let [dy (- r (first pt))
        dx (- c (second pt))]
    (cond (or (= 0 dy dx)
              (not= \# (nth (nth board r) c))
              (util/only-or (= 0 dx) (= 0 deltax))
              (and (= 0 dx deltax) (util/diff-signs dy deltay)))
          false
          (and (or (= 0 dx deltax)
                       (= (/ dy dx) (/ deltay deltax)))
               (or (> (Math/abs deltax) (Math/abs dx))
                   (> (Math/abs deltay) (Math/abs dy))))
          true
          :else false)))

(defn is-visible-from [board row col pt]
  (let [deltay (- row (first pt))
        deltax (- col (second pt))]
    (not-any? (partial is-blocked-by board pt deltax deltay)
              (map vec (util/prod (range (min (first pt) row)
                                         (inc (max (first pt) row)))
                                  (range (min (second pt) col)
                                         (inc (max (second pt) col))))))))

(defn num-visible [board pt]
  (apply +
         (for [row (range (count board))
               col (range (count (first board)))]
           (if (and (= \# (nth (nth board row) col))
                    (not (and (= row (first pt)) (= col (second pt))))
                    (is-visible-from board row col pt))
             1 0))))

(defn most-visible [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (let [board (line-seq rdr)]
      (reduce #(if (>= (first %1) (first %2)) %1 %2)
              (for [row (range (count board))
                    col (range (count (first board)))]
                `(~(if (= \# (nth (nth board row) col))
                     (num-visible board `(~row ~col))
                     0)
                  ~row
                  ~col))))))


