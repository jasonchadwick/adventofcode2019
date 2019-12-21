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

; argument number for each opcode (index is opcode mod 10)
(def argnums [0 3 3 1 1 2 2 3 3 0])

(defn run-code [v pos inputs]
  (if (string? v) (recur (make-vec v) pos inputs)
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
            outputting (== 0 (cond
                               (<= 1 opcode 2) (nth args 2)
                               (== opcode 3) (nth args 0)
                               (== opcode 4) 0
                               (<= 5 opcode 6) 1
                               (<= 7 opcode 8) (nth args 2)
                               (== opcode 99) 0))
            final (== opcode 99)
            newv (case opcode
                   1 (assoc v (nth args 2) (+ (valfn 0) (valfn 1)))
                   2 (assoc v (nth args 2) (* (valfn 0) (valfn 1)))
                   3 (assoc v (nth args 0) (first inputs))
                   4 (assoc v 0 (nth v (nth args 0)))
                   5 v
                   6 v
                   7 (assoc v (nth args 2)
                            (if (< (valfn 0) (valfn 1)) 1 0))
                   8 (assoc v (nth args 2)
                            (if (== (valfn 0) (valfn 1)) 1 0))

                   99 v)
            newpos (case opcode
                     5 (if (zero? (valfn 0)) (+ 3 pos) (valfn 1))
                     6 (if (zero? (valfn 0)) (valfn 1) (+ 3 pos))
                     (+ 1 arg-ct pos))
            newinputs (if (== opcode 3) (util/safe-cdr inputs nil) inputs)]
        (cond
          (and outputting final) (first newv)
          outputting `(~newv ~newpos ~newinputs)
          :else (recur newv newpos newinputs)))))

;(defn run-from-file [fname pos inputs]
;    (run-code (make-vec fname) pos inputs))

(defn run-to-end [v pos inputs]
  (let [output (run-code v pos inputs)]
    (if (number? output) output
        (recur (first output) (second output) (nth output 2)))))

;(defn run-to-end-f [fname pos inputs]
;  (run-to-end (make-vec fname) pos inputs))