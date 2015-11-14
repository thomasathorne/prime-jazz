(ns prime-jazz.synth
  (:require [overtone.core :refer :all]))

(defn gen-harms
  [r n]
  (mapv #(Math/pow r %) (range n)))

(definst oboe
  [attack 0.01 release 0.2 freq 440 vol 0.9 dur 3]
  (let [env      (env-gen:kr (lin attack dur release) 1 1 0 1 FREE)
        vib-size (env-gen:kr (lin 2 dur 0.2) 1 3 5)
        vib      (sin-osc:kr vib-size)]
    (resonz
     (resonz
      (resonz
       (* 40 vol env
          (+ 1 (* 0.1 vib))
          (apply + (map (fn [i h]
                          (* h (sin-osc (* i freq (+ 1 (* vib 0.007))))))
                        (range)
                        [1 0 0.9 0 0.81 0 0.75 0 0.6 0
                         0.4 0 0.3 0 0.4 0 0.3 0 0.1 0 0.0 0
                         0.1 0 0.03 0 0.05 0 0.06])))
       850 0.1)
      1610.0 0.1)
     3500 0.1)))

(definst hit
  [freq 440 dur 0.4]
  (let [env (env-gen:kr (perc 0.01 dur) 1 1 0 1 FREE)]
    (resonz
     (resonz
      (resonz
       (resonz
        (+ (* env
              env
              env
              (hpf (white-noise) (* 2 freq)))
           (* env
              (apply + (map (fn [i h]
                              (* h (sin-osc (* i freq))))
                            (range)
                            [1 0.95 0.9 0.89 0.81 0.3 0.75 0.1 0.6 0.2
                             0.4 0.1 0.3 0.2 0.3 0.1 0.3 0.2 0.1 0.04 0.0 0.1
                             0.1 0.1 0.03 0.05]))))
        800 1)
       3440 1)
      1593 1)
     409 1)))
