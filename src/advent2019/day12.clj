(ns advent2019.day12
  (:require [clojure.java.io]))

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

(total-energy-after-n "resources/day12.txt" 1000)