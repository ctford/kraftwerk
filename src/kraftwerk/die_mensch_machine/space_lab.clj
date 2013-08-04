(ns kraftwerk.die-mensch-machine.space-lab
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.chord]
    [leipzig.canon]
    [leipzig.scale])
  (:require
    [overtone.live :as overtone]))

; See https://github.com/supercollider/supercollider/blob/master/examples/pieces/spacelab.scd

; Utilities
(defn mapthen [f & args]
  (->> args
       (apply map f)
       (reduce #(then %2 %1))))

(defn but
  "Replace the specified section of notes.
  e.g. (->> melody (but :from 3 :until 7 (phrase [3 4] [5 5])))
       (->> melody (but :from 3 (phrase [4] [5])))
       (->> melody (but :until 7 (phrase [1] [3])))"
  [& args]
  (let [[replacement notes] (take-last 2 args)
        {start :from end :until :or {start 0 end Integer/MAX_VALUE}} args]
    (reduce with
            [(->> notes (take-while (fn [{t :time}] (< t start))))
             (->> replacement (where :time (from start))) 
             (->> notes (drop-while (fn [{t :time}] (< t end))))])))

; Notes
(def beat
  (->> (rhythm (repeat 32 1/4))
       (where :drum (is :tick))
       (with (->> (rhythm (repeat 8 1)) (having :drum (cycle [:kick :tock]))))
       (where :part (is :beat))))

(def progression
  [(-> triad (root 2) (inversion 1))
   (-> triad (root -4) (update-in [:iii] #(+ % 1/2)))]) 

(def chorus
  (let [chords   (->> (interleave (repeat nil) progression)
                      (phrase (cycle [1 3]))
                      (where :part (is :chords))) 
        melodya  (->> [0 2 3 0]
                      (phrase [1/2 7/2 1/2 8/2])
                      (after -1/2)
                      (where :part (is :melody)))
        melodyb  (->> melodya
                      (but :from 4 (phrase [8/2] [7]))
                      (where :part (is :melody)))
        harmony  (->> (phrase [8] [7])
                      (where :part (is :harmony)))
        echo     (->> (mapcat repeat [16 16] [2 0])
                      (phrase (repeat 1/4))
                      (where :part (is :echo)))
        accompaniment (reduce with [echo chords harmony beat])] 
    (->> accompaniment
         (then (with accompaniment melodya))
         (then accompaniment)
         (then (with accompaniment melodyb)))))

(def intro
  (let [line (->> (phrase (repeat 1) (range 2 16))
                  (canon (simple 1/2))
                  (then (after -1/2 (phrase [1] [[2 9 16]])))
                  (where :duration #(* % 8))
                  (where :part (is :echo)))]
    (->> line 
         (then (->> (filter (comp #{:melody} :part) chorus)
                    (with (->> (interleave progression (repeat nil))
                               (phrase (cycle [3 1]))
                               (times 4 8)
                               (where :part (is :chords)))))) 
         (then (->> chorus
                    (but :from 16 [])
                    (filter (comp #{:echo :beat} :part)))))))

(def verse
  (let [bass     (->> [0 2 3 6 7]
                      (phrase [7.5 0.5 3.5 0.5 4])
                      (where :pitch lower)
                      (where :part (is :bass)))
        melodya  (->> [5 6 7 6 5 4 5 6]
                      (phrase [1 1/2 3/2 1/2 1/2 1 1/2 5/2])
                      (canon (comp (interval -2) (simple 8)))
                      (where :part (is :melody)))
        melodyb  (->> melodya 
                      (but :from 3 :until 4 (phrase [1/2 1/2] [8 9]))
                      (but :from 13 (phrase [1/2 5/2] [-1 0]))
                      (where :part (is :melody)))
        harmony  (->> [7 6 5 4]
                      (phrase (repeat 4))
                      (where :part (is :harmony)))
        echo     (->> (mapcat repeat [32 14 2 16] [0 3 6 0])
                      (phrase (repeat 1/4))
                      (where :part (is :echo)))
        accompaniment (reduce with [bass harmony echo (times 2 beat)])]
    (->> (with melodya accompaniment)
         (then (with melodyb accompaniment)))))

(def break
  (let [harmony (->> (phrase (repeat 32 1) (cycle [0 7]))
                     (where :part (is :harmony)))
        bass    (->> (phrase (repeat 4 8) (cycle [2 0]))
                     (where :pitch lower)
                     (where :part (is :bass)))
        melodya (->> (phrase [16 7 1 8] [nil 2 3 0])
                     (where :part (is :melody)))
        melodyb (->> melodya
                     (but :from 24 (phrase [8] [7]))
                     (where :part (is :melody))) 
        echo    (->> (mapcat repeat [32 32] [2 0])
                     (phrase (repeat 1/4))
                     (times 2)
                     (where :part (is :echo)))]
    (->> (reduce with [melodya bass harmony])
         (then (reduce with [melodyb harmony echo bass (times 4 beat)])))))

(def vamp 
  (let [extra-harmony (->> [7 2 3 4 2 3 7]
                           (phrase [4 1 0.5 6.5 1 0.5 2.5])
                           (times 2)
                           (where :part (is :harmony)))
        once (with verse extra-harmony)
        fade-out (map (fn [{t :time :as note}]
                        (assoc note :volume (/ (- 32 t) 32)))
                      once)]
    (->> once 
         (times 2)
         (then fade-out))))

(def track
  (->> intro
       (then chorus)
       (then verse)
       (then chorus)
       (then break)
       (then verse)
       (then chorus)
       (then vamp)
       (wherever :pitch, :pitch (comp overtone/midi->hz C minor)) 
       (wherever (comp not :volume), :volume (is 1.0))
       (where :duration (bpm 105)) 
       (where :time (bpm 105))))

; Instruments
(overtone/definst hat [vol 1.0] 
  (-> (overtone/perc 0 0.05)
      (overtone/env-gen :action overtone/FREE)
      (* vol (overtone/white-noise)))) 

(overtone/definst snare [vol 1.0]
  (-> (overtone/perc 0 0.05)
      (overtone/env-gen :action overtone/FREE)
      (* vol (overtone/mix [(overtone/sin-osc 200) (overtone/white-noise)]))))

(overtone/definst bass-drum [vol 1.0]
  (-> (overtone/perc 0 0.05)
      (overtone/env-gen :action overtone/FREE)
      (* 3 vol (overtone/sin-osc 40))))

(overtone/definst bass [freq 440 dur 1000 vol 1.0]
  (let [envelope (overtone/env-gen (overtone/asr 0 1 1)
                                   (overtone/line:kr 1.0 0.0 (/ dur 1000))
                                   :action overtone/FREE)
        level (+ 100 (overtone/env-gen (overtone/perc 0 3)
                                       :level-scale 6000))
        osc (overtone/mix [(overtone/saw freq)
                           (overtone/saw (* freq 1.005))
                           (overtone/pulse (/ freq 2) 0.5)])]
    (-> osc (overtone/lpf level) (* vol envelope))))

(overtone/definst solo [freq 440 dur 1000 vol 1.0]
  (let [envelope (overtone/env-gen (overtone/asr 0.2 1 2)
                                   (overtone/line:kr 1.0 0.0 (/ dur 1000))
                                   :action overtone/FREE)
        level (+ 600 (overtone/env-gen (overtone/adsr 0.4 1.2 0.7 2)
                                :level-scale 800))
        osc (overtone/mix
              [(overtone/saw (overtone/lag freq 0.1))
               (overtone/saw (overtone/lag (* freq 1.005) 0.1))])]
    (-> osc (overtone/lpf level) (* 2 vol envelope))))

(overtone/definst string [freq 440 dur 1000 vol 1.0]
  (let [envelope (overtone/env-gen (overtone/asr 0.2 1 0.5)
                                   (overtone/line:kr 1.0 0.0 (/ dur 1000))
                                   :action overtone/FREE)
        osc (overtone/saw freq)]
    (-> osc (overtone/lpf 1500) (* 1/2 vol envelope))))

(defonce sweep-bus (overtone/audio-bus))
(defonce sweepers (overtone/group "Synths connected to the sweep."))
(defonce writers (overtone/group "Pushing onto the bus." :head sweepers))
(defonce readers (overtone/group "Pulling from the bus." :after writers))

(overtone/defsynth random-walk [out-bus 0 freq 0.3]
  (overtone/out:kr out-bus (overtone/mul-add (overtone/lf-noise1:kr freq) 1000 1500)))

(defonce sweep (random-walk :tgt writers sweep-bus))

(overtone/defsynth res [freq 440 dur 350 vol 1.0]
  (overtone/out 0 (-> (overtone/perc 0 (* 2 (/ dur 1000)))
                      (overtone/env-gen :action overtone/FREE)
                      (* (overtone/mix [(overtone/saw freq) (overtone/pulse (/ freq 2) 0.5)]))
                      (overtone/rlpf (overtone/in:kr sweep-bus) 0.1)
                      (* 1/2 vol))))

(overtone/definst poly [freq 440 dur 1000 vol 1.0]
  (let [envelope (overtone/env-gen (overtone/asr 0.2 1 0.1)
                                   (overtone/line:kr 1.0 0.0 (/ dur 1000))
                                   :action overtone/FREE)
        level (overtone/env-gen (overtone/asr 7 1 0.2)
                                :level-scale 12000)
        osc1 (overtone/mix
               (map
                 #(overtone/pulse (* freq %) (overtone/mul-add (overtone/lf-cub 2) 0.3 0.5))
                 [1 1.007]))
        osc2 (overtone/pulse (/ freq 2) 0.3)]
    (* 1/3 vol envelope (overtone/rlpf (+ osc1 osc2) level 0.9))))

; Arrangement
(def kit {:kick bass-drum
          :tick hat
          :tock snare})

(defmethod play-note :beat [{:keys [drum volume]}]
  ((drum kit) volume))

(defmethod play-note :bass [{:keys [pitch duration volume]}]
  (bass pitch duration volume))

(defmethod play-note :melody [{:keys [pitch duration volume]}]
  (solo pitch duration volume))

(defmethod play-note :harmony [{:keys [pitch duration volume]}]
  (string (* 2 pitch) duration volume)) 

(defmethod play-note :echo [{:keys [pitch duration volume]}]
  (res :tgt readers pitch duration volume))

(defmethod play-note :chords [{:keys [pitch duration volume]}]
  (poly pitch duration volume))

(comment
  (play track)
  )
