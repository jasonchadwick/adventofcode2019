(ns advent2019.intcode
  (:require [advent2019.util :as util])
  (:require [clojure.java.io])
  (:require [clojure.string]))

; Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.
; Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.
; Opcode 3 takes a single integer as input and saves it to the position given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
; Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.
; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
; Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
; Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
; Opcode 9 adjusts the relative base by the value of its only parameter. The relative base increases (or decreases, if the value is negative) by the value of the parameter.
; Opcode 99 means that the program is finished and should immediately halt.

(defn make-vec [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (vec (map #(Long/parseLong %) (clojure.string/split (slurp rdr) #",")))))

; creates a function which gives the nth argument's value
; (output function should only be used to get *VALUES* not output indices)
(defn make-val-fn [v args arg-modes]
  (fn [rel n]
    (case (nth arg-modes n)
      0 (nth v (nth args n))
      1 (nth args n)
      2 (nth v (+ rel (nth args n))))))

; same idea as val-fn above, but used when we know we want a 
; position, not a value
(defn make-pos-fn [args arg-modes]
  (fn [rel n]
    (case (nth arg-modes n)
      2 (+ rel (nth args n))
      (nth args n))))

; argument number for each opcode (index is opcode mod 20)
(def argnums [0 3 3 1 1 2 2 3 3 1
              0 0 0 0 0 0 0 0 0 0])

; TODO: add support for "unbounded array" style memory
; not sure why it doesnt work for [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
;                 supposed to "self-replicate"
(defn code-step [v pos rel inputs]
  ;(println v pos rel inputs)
  (let [opcode (mod (nth v pos) 100)
        arg-ct (nth argnums (mod opcode 10))
        args (map #(nth v (+ 1 pos %))
                  (range arg-ct))
        modes ((fn get-modes [code n max]
                 (if (>= n max) nil
                     (cons (util/digit code (+ 2 n)) (get-modes code (inc n) max))))
               (nth v pos) 0 arg-ct)
        valfn (make-val-fn v args modes)
        posfn (make-pos-fn args modes)
        outputting (== 0 (cond
                           (<= 1 opcode 2) (posfn rel 2)
                           (== opcode 3) (posfn rel 0)
                           (== opcode 4) 0
                           (<= 5 opcode 6) 1
                           (<= 7 opcode 8) (posfn rel 2)
                           (== opcode 9) 1
                           (== opcode 99) 0))
        final (== opcode 99)
        ;test (println opcode args)
        newv (case opcode
               1 (assoc v (posfn rel 2) (+ (valfn rel 0) (valfn rel 1)))
               2 (assoc v (posfn rel 2) (* (valfn rel 0) (valfn rel 1)))
               3 (assoc v (posfn rel 0) (first inputs))
               4 (assoc v 0 (valfn rel 0))
               5 v
               6 v
               7 (assoc v (posfn rel 2)
                        (if (< (valfn rel 0) (valfn rel 1)) 1 0))
               8 (assoc v (posfn rel 2)
                        (if (== (valfn rel 0) (valfn rel 1)) 1 0))
               9 v
               99 v)
        newpos (case opcode
                 5 (if (zero? (valfn rel 0)) (+ 3 pos) (valfn rel 1))
                 6 (if (zero? (valfn rel 0)) (valfn rel 1) (+ 3 pos))
                 (+ 1 arg-ct pos))
        newrel (if (== opcode 9) (+ rel (valfn rel 0)) rel)
        newinputs (if (== opcode 3) (util/safe-cdr inputs nil) inputs)]
    (cond
      (and outputting final) (first newv)
      outputting `(~newv ~newpos ~newrel ~newinputs)
      :else (recur newv newpos newrel newinputs))))

(defn run-code [v pos rel inputs init]
  (let [code (if (string? v) (make-vec v) v)]
    (code-step (if init 
                 (vec (concat code (util/repeated-seq 0 1000)))
                 code)
               pos rel inputs)))

(defn run-to-end [v pos rel inputs]
  (loop [output (code-step (vec (concat (if (string? v) (make-vec v) v)
                                        (util/repeated-seq 0 1000)))
                           pos rel inputs)]
    (if (number? output) output
        (recur (apply code-step output)))))