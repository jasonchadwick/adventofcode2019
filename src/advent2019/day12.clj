(ns advent2019.day12
  (:require [clojure.java.io])
  (:require [advent2019.util :as util]))

(def moons '([0 0 0 6 -2 -7]
             [0 0 0 -6 -6 -4]
             [0 0 0 -9 11 0]
             [0 0 0 -3 -4 6]))

; each body represented by a vector: [vx, vy, vz, x, y, z]
(defn v-x [m]
  (first m))
(defn v-y [m]
  (second m))
(defn v-z [m]
  (nth m 2))
(defn x [m]
  (nth m 3))
(defn y [m]
  (nth m 4))
(defn z [m]
  (nth m 5))

(defn accel [x1 x2]
  (cond (> x2 x1) 1
        (< x2 x1) -1
        (= x2 x1) 0))

(defn do-step [moons]
  (for [moon1 moons]
    (let [moon-new (loop [m1 moon1
                          m2s (remove #(= m1 %) moons)]
                     (if (empty? m2s) m1
                         (let [m2 (first m2s)]
                           (recur (assoc m1
                                         0 (+ (v-x m1) (accel (x m1) (x m2)))
                                         1 (+ (v-y m1) (accel (y m1) (y m2)))
                                         2 (+ (v-z m1) (accel (z m1) (z m2)))
                                         3 (x m1)
                                         4 (y m1)
                                         5 (z m1))
                                  (rest m2s)))))]
      (assoc moon-new 
             3 (+ (x moon-new) (v-x moon-new))
             4 (+ (y moon-new) (v-y moon-new))
             5 (+ (z moon-new) (v-z moon-new))))))

(defn n-steps [n ms]
  (loop [counter 0
         ms ms]
    (if (= counter n) (vec ms)
        (recur (inc counter) (do-step ms)))))

(defn steps-from-file [fname n]
  (with-open [rdr (clojure.java.io/reader fname)]
    (n-steps n (map #(vec 
                     (concat 
                      '(0 0 0)
                      (loop [s % acc nil] 
                        (cond (empty? s) (reverse acc)
                              (or (= (first s) \x)
                                  (= (first s) \y)
                                  (= (first s) \z))
                              (recur (subs s 5)
                                     (cons (Integer/parseInt (subs s 2 5)) acc))
                              :else (recur (subs s 1) acc)))))
                   (line-seq rdr)))))

(defn total-energy [m]
  (* (apply + (map #(Math/abs %) (subvec m 3)))
     (apply + (map #(Math/abs %) (subvec m 0 3)))))

(defn total-energy-after-n [fname n]
  (apply + (map #(total-energy %) (steps-from-file fname n))))


; part II

; axis state format: [v_x_1, x_1, v_x_2, x_2, ..., v_x_n, x_n]
; even indices are velocities, odd are positions.
(defn get-axis-state [ms axis]
  (let [idxs (case axis
               x '(0 3)
               y '(1 4)
               z '(2 5))]
    (vec (flatten (for [m ms] `(~(nth m (first idxs)) ~(nth m (second idxs))))))))

(defn axis-step [ms]
  (let [accel-sum #(apply + (for [i '(1 3 5 7) :when (not= % i)]
                              (accel (nth ms %) (nth ms i))))]
    (-> ms
        (#(assoc %
                 0 (+ (nth % 0)
                      (accel-sum 1))
                 2 (+ (nth % 2)
                      (accel-sum 3))
                 4 (+ (nth % 4)
                      (accel-sum 5))
                 6 (+ (nth % 6)
                      (accel-sum 7))))
        (#(assoc %
                 1 (+ (nth % 0) (nth % 1))
                 3 (+ (nth % 2) (nth % 3))
                 5 (+ (nth % 4) (nth % 5))
                 7 (+ (nth % 6) (nth % 7)))))))

; finds next point where velocity is zero - twice this number is
; the period on the given axis
(defn find-repeated-axis-state [ms states]
  (let [new-states (inc states)
        new-ms (axis-step ms)]
    (if (and (zero? (nth new-ms 0)) (zero? (nth new-ms 2)) (zero? (nth new-ms 4)) (zero? (nth new-ms 6)))
      (* 2 new-states)
      (recur new-ms new-states))))

(defn axis-period [axis fname]
  (let [ms (get-axis-state (steps-from-file fname 0) axis)]
    (case axis
      x (find-repeated-axis-state ms 0)
      y (find-repeated-axis-state ms 0)
      z (find-repeated-axis-state ms 0))))

(defn find-period [fname]
  (util/lcm (axis-period 'x fname)
            (axis-period 'y fname)
            (axis-period 'z fname)))

`(~(total-energy-after-n "resources/day12.txt" 1000)
  ~(find-period "resources/day12.txt"))