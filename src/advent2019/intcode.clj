;intcode functions, used by several days

(ns advent2019.intcode
  (:require [advent2019.util :as util])
  (:require [clojure.java.io])
  (:require [clojure.string]))

(defn make-vec [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (vec (map (fn [s] (Integer/parseInt s)) (clojure.string/split (slurp rdr) #",")))))

; creates a function which gives the nth argument's value
; (output function should only be used to get *VALUES* not output indices)
(defn make-val-fn [v args arg-modes]
  (fn [n] (if (== 0 (nth arg-modes n))
            (nth v (nth args n))
            (nth args n)))) 

; argument number for each opcode (opcode mod 10)
(def argnums [0 3 3 1 1 2 2 3 3 0])

(defn output [v]
  (nth v 0))

; todo: input is a list. if nil, then input -> (output v)
(defn run-code-1 [v inputs pos]
  (if (>= pos (count v))
    (throw (Exception. "cursor too large"))
    (let [opcode (mod (nth v pos) 100)
          arg-ct (nth argnums (mod opcode 10))
          args (map (fn [n] (nth v (+ 1 n pos)))
                    (range arg-ct))
          valfn (make-val-fn
                 v args
                 ((fn get-modes [code n max]
                    (if (>= n max) nil
                        (cons (util/digit code (+ 2 n)) (get-modes code (inc n) max))))
                  (nth v pos) 0 arg-ct))
          input (if (empty? inputs) (output v) (first inputs))
          newv (case opcode
                 1 (assoc v (nth args 2) (+ (valfn 0) (valfn 1)))
                 2 (assoc v (nth args 2) (* (valfn 0) (valfn 1)))
                 3 (assoc v (nth args 0) input)
                 4 (assoc v 0 (nth v (nth args 0)))
                 5 (if (zero? (valfn 0)) v `(~v ~(valfn 1)))
                 6 (if (zero? (valfn 0)) `(~v ~(valfn 1)) v)
                 7 (assoc v (nth args 2)
                          (if (< (valfn 0) (valfn 1)) 1 0))
                 8 (assoc v (nth args 2)
                          (if (== (valfn 0) (valfn 1)) 1 0))

                 99 (output v))]
      (cond
        (number? newv) newv
        (vector? newv) (recur newv 
                              (util/safe-cdr inputs nil) (+ 1 arg-ct pos))
        (coll? newv) (recur (first newv) 
                            (util/safe-cdr inputs nil) (second newv))))))

(defn run-code [v & inputs]
  (run-code-1 v inputs 0))

(defn run-from-file [fname & inputs]
  (let [v (make-vec fname)]
    (run-code-1 v inputs 0)))