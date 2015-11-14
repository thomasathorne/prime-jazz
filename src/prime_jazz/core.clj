(ns prime-jazz.core
  (:require [overtone.core :refer :all]
            [prime-jazz.synth :refer :all]
            [prime-jazz.melody :refer :all]))

(definst saw-pulse [freq 440 dur 0.4]
  (let [env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)]
    (resonz
     (resonz
      (* 2 env (saw freq))
      2030 0.3)
     877 0.4)))

(defn pul [inst n]
  (inst (minor-scale 100 n)))

(def primes
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251])

(def factors
  (memoize
   (fn [n]
     (loop [ps primes]
       (cond
        (empty? ps)
        []

        (== 0 (rem n (first ps)))
        (conj (factors (quot n (first ps))) (first ps))

        :else
        (recur (rest ps)))))))

(defn play-chord
  [ps]
  (mapv
   (comp #(pul saw-pulse %)
         (zipmap primes (range)))
   ps))

(def m (metronome 500))

(defn prime-jazz
  [beat metro n]
  (at (metro beat) (play-chord (factors n)))
  (apply-by (metro (inc beat))
            #'prime-jazz
            [(inc beat) metro (inc n)]))

;(prime-jazz (m) m 1)

;(prime-jazz (m) m 4000)

;(stop)
