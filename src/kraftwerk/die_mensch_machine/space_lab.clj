(ns kraftwerk.die-mensch-machine.space-lab
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.chord]
    [leipzig.canon]
    [leipzig.scale])
  (:require
    [overtone.live :as overtone]))

; Utilities
(defn mapthen  [f & args]
  (->> args
       (apply map f)
       (reduce #(then %2 %1))))

(def inv inversion)

; Notes

(def beat
  (->>
    (rhythm (repeat 32 1/4)) (where :drum (is :tick))
    (with (->> (rhythm (repeat 8 1)) (having :drum (cycle [:kick :tock]))))
    (where :part (is :beat))))

(def track
  (->>
    beat
    (wherever :pitch, :pitch (comp C minor)) 
    (where :duration (bpm 105)) 
    (where :time (bpm 105))))

; Instruments
(overtone/definst hat  [] 
  (-> (overtone/perc 0 0.05)
      (overtone/env-gen :action overtone/FREE)
      (* (overtone/white-noise)))) 

(overtone/definst snare []
  (-> (overtone/perc 0 0.05)
      (overtone/env-gen :action overtone/FREE)
      (* (overtone/mix [(overtone/sin-osc 200) (overtone/white-noise)]))))

(overtone/definst bass-drum []
  (-> (overtone/perc 0 0.05)
      (overtone/env-gen :action overtone/FREE)
      (* 3 (overtone/sin-osc 40))))

; Arrangement
(def kit {:kick bass-drum
          :tick hat
          :tock snare})
(defmethod play-note :beat [{drum :drum}] ((drum kit)))
