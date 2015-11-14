(ns prime-jazz.melody
  (:require [overtone.core :refer :all]))

(defn minor-scale [base n]
  (let [octs   (quot n 7)
        degree (rem n 7)]
    (* base
       (Math/pow 2 octs)
       ({0 1
         1 (/ 9 8)
         2 (/ 32 27)
         3 (/ 4 3)
         4 (/ 3 2)
         5 (/ 128 81)
         6 (/ 16 9)} degree))))

(defn major-scale [base n]
  (let [octs   (quot n 7)
        degree (rem n 7)]
    (* base
       (Math/pow 2 octs)
       ({0 1
         1 (/ 9 8)
         2 (/ 5 4)
         3 (/ 4 3)
         4 (/ 3 2)
         5 (/ 27 16)
         6 (/ 15 8)} degree))))

(defn play-tune
  ([tune inst tempo scale]
   (let [m (metronome tempo)]
     (play-tune tune inst tempo scale 0 m)))
  ([tune inst tempo scale beat metronome]
   (mapv (fn [i n]
           (at (metronome (+ i beat)) (inst 0 0 (scale 110 n) 1 (/ 60 tempo))))
         (range)
         tune)))

(defn play-canon
  [tune inst tempo scale parts separation]
  (let [m (metronome tempo)]
   (mapv (fn [i]
           (at (m i) (play-tune tune inst tempo scale i m)))
         (range 0 (* parts separation) separation))))

(def grace [7 7 6 7 7 8 8 9
            7 10 10 9 9 8 8 7
            11 10 8 9 9 8 8 7
            4 5 6 7 9 8 8 7])

(def london-burning [4 4 7 7 7 7 4 4 7 7 7 7
                     8 8 9 9 9 9 8 8 9 9 9 9
                     11 11 11 11 11 11 11 11 11 11 11 11
                     4 4 7 7 7 7 4 4 7 7 7 7])
