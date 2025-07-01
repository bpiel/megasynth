(ns megasynth.bill-v1
  (:require [overtone.live :as o]
            [overtone.midi :as midi])
  (:import [javax.sound.midi MidiDevice MidiDevice$Info MidiSystem
            Receiver Sequencer ShortMessage Synthesizer SysexMessage Transmitter]))

(def synth-cache& (atom {}))
(def patch-states& (atom {}))

(defonce midi-msg-fn& (atom identity))

(defonce current-midi& (atom nil))

(defonce current-synth& (atom nil))

(defonce synths& (atom {}))

(def notes& (atom {}))

(defn midi->hz [note]
  (* 440 (Math/pow 2 (/ (- note 69) 12.0))))

(defn play [synth dur-ms & args]
  (let [vc (apply synth args)]
    (future
      (Thread/sleep dur-ms)
      (o/ctl vc :gate 0.0))
    true))

(defn set-synth! [s]
  (reset! current-synth& s))

(defn scale-knob-value
  "Convert 0.0-1.0 knob value to actual parameter range"
  [knob-value min-val max-val curve]
  (case curve
    :linear (+ min-val (* knob-value (- max-val min-val)))
    :exponential (+ min-val (* (Math/pow knob-value 2) (- max-val min-val)))
    :logarithmic (+ min-val (* (Math/sqrt knob-value) (- max-val min-val)))
    (+ min-val (* knob-value (- max-val min-val)))))

(defmacro mk-synth* [args body]
  `(o/synth ~(into ['freq 440
                    'gate 1]
                   (mapcat (fn [[a [_ v]]] [a v])
                           (partition 2 args)))
     ~body))

(macroexpand-1
 ')

(mk-synth* [amp [0.01 1.0 3.0]
              dur [0.01 1.0 10.0]
              cutoff [1 2200 6000]
              boost [0 12 24]
              dist-level [0.001 0.015 0.2]]
   (let [env (o/env-gen (o/adsr 0.3 0.7 0.5 0.3) (o/line:kr 1.0 0.0 dur) :action o/FREE)
         level (+ (* freq 0.25)
                  (o/env-gen (o/adsr 0.5 0.3 1 0.5) (o/line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff))
         osc (o/mix [(o/saw freq)
                     (o/saw (* freq 0.7491535384383409))])
         sig (-> osc
                 (o/bpf level 0.6)
                 (* env amp)
                 o/pan2
                 (o/clip2 dist-level)
                 (* boost)
                 o/distort)]
     (o/out 0 sig)))

(defmacro mk-synth [args body]
  {:arg-specs args
   :synth (mk-synth* args body)})

(mk-synth
 [amp [0.01 1.0 3.0]
  dur [0.01 1.0 10.0]
  cutoff [1 2200 6000]
  boost [0 12 24]
  dist-level [0.001 0.015 0.2]]
 (let [env (o/env-gen (o/adsr 0.3 0.7 0.5 0.3) (o/line:kr 1.0 0.0 dur) :action o/FREE)
       level (+ (* freq 0.25)
                (o/env-gen (o/adsr 0.5 0.3 1 0.5) (o/line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff))
       osc (o/mix [(o/saw freq)
                   (o/saw (* freq 0.7491535384383409))])
       sig (-> osc
               (o/bpf level 0.6)
               (* env amp)
               o/pan2
               (o/clip2 dist-level)
               (* boost)
               o/distort)]
   (o/out 0 sig)))


#_
(reset! midi-msg-fn&
        (fn [e]
          (def e1 e)
          (clojure.pprint/pprint e)))



(def default-synth1
  (o/synth [freq 440
            amp 0.7
            attack 0.1
            decay 0.9
            sustain 1.0
            release 0.9
            gate 1.0]
    (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
          sig (* amp env (o/square freq))]
      (o/out 0 (o/pan2 sig)))))


(def default-synth2
  (o/synth [freq 440
            amp 0.5
            attack 0.1
            decay 0.9
            sustain 1.0
            release 0.9
            gate 1.0]
    (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
          x (o/mix [(o/sin-osc freq)
                    (o/sin-osc (* 1.5 freq))
                    (o/sin-osc (* 2 freq))])
          sig (* 2.0 amp env x)]
      (o/out 0 (o/pan2 sig)))))

(def default-synth3
  (o/synth [freq 440
            amp 0.3
            attack 0.1
            decay 0.0
            sustain 1.0
            release 0.0
            gate 1.0]
    (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/NO-ACTION)
          env2 (o/env-gen (o/adsr attack 1.0 sustain 1.0) gate :action o/FREE)
          sig1 (* amp env (o/sin-osc freq))
          sig2 (* amp env2 (o/sin-osc (* 1.5 freq)))
          x (+ sig1 sig2)]
      (o/out 0 x))))

(default-synth2)

(o/stop)

#_(play default-synth2 400)


(o/defsynth da-funk0
  [freq 440 dur 1.0 amp 1.0 cutoff 2200 boost 12 dist-level 0.015]
  (let [env (o/env-gen (o/adsr 0.3 0.7 0.5 0.3) (o/line:kr 1.0 0.0 dur) :action o/FREE)
        level (+ (* freq 0.25)
                 (o/env-gen (o/adsr 0.5 0.3 1 0.5) (o/line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff))
        osc (o/mix [(o/saw freq)
                    (o/saw (* freq 0.7491535384383409))])
        sig (-> osc
                (o/bpf level 0.6)
                (* env amp)
                o/pan2
                (o/clip2 dist-level)
                (* boost)
                o/distort)]
    (o/out 0 sig)))

(o/defsynth da-funk-cgpt0
  [freq 440 dur 1.0 amp 1.0 cutoff 2200 boost 12 dist-level 0.015]
  (let [env (o/env-gen (o/adsr 0.01 0.1 0.8 0.05) (o/line:kr 1.0 0.0 dur) :action o/FREE)
        level (+ (* freq 0.25)
                 (o/env-gen (o/adsr 0.2 0.1 1 0.2) (o/line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff))
        osc (o/mix [(o/saw freq)
                    (o/saw (* freq 0.7491535384383409))])
        sig (-> osc
                (o/bpf level 0.6)
                (* env amp)
                o/pan2
                (o/clip2 dist-level)
                (* boost)
                o/distort)]
    (o/out 0 sig)))

(defonce *da-funk0-voice (atom nil))

(defn da-funk-mono [& args]
  (when-let [s @*da-funk0-voice]
    (when-not (-> s :status deref (= :destroyed))
      (o/kill s))) ;; Stop previous voice
  (reset! *da-funk0-voice
          (apply da-funk0 args)))

(defn ->mono [syn]
  (let [mono& (atom nil)]
    (fn [& args]
      (when-let [s @mono&]
        (when-not (-> s :status deref (= :destroyed))
          (o/kill s))) 
      (reset! mono&
              (apply syn args)))))

(def da-funk-mono0 (->mono da-funk0))

#_
(do
  (da-funk-mono 220)
  (Thread/sleep 300)
  (da-funk-mono 260))


(defn note-on! [note-id]
  (try
    (let [freq (midi->hz note-id)
          node1 (@current-synth& :freq freq)]
      (swap! notes& assoc note-id node1)
      node1)
    (catch Throwable e
      (clojure.pprint/pprint e))))

(defn note-off! [note-id]
  (when-let [node0 (@notes& note-id)]
    (try
      (when-not (-> node0 :status deref (= :destroyed))
        (o/ctl node0 :gate 0.0))
      (catch Throwable e
        (def node00 node0)
        (def node00-state (-> node0 :status deref))
        (clojure.pprint/pprint e)
        (clojure.pprint/pprint node0))))
  true)

(defn control-change! [{:keys [command note status data1 data2 velocity] :as msg}]
  (case data1
    14 (set-synth! da-funk-mono)
    15 (set-synth! (->mono fc-lead-x4))))

(defn short-print-midi-msg [{:keys [command note status data1 data2 velocity] :as msg}]
  (format "cmd %s, status %s, note %d, vel %d, data [%d %d]\n"
          command status note velocity data1 data2))

(defn midi-handler [{:keys [command note] :as msg}]
  #_(clojure.pprint/pprint msg)
  (println (short-print-midi-msg msg))
  (case command
    :note-on (note-on! note)
    :note-off (note-off! note)
    :control-change (control-change! msg)
    nil
    #_(clojure.pprint/pprint msg)))

#_(midi-handler {:command :note-on :note 60})
#_(midi-handler {:command :note-off :note 60})

{:patch-name {:dev1 {:k {:type :lfo
                         :inputs {:freq :k1 :amp :k2}}
                     :dev [:SYNTH-GOES-HERE :SYNTH-GOES-HERE] ;; knobs have no device
                     :out-bus [:BUS-GOES-HERE :BUS-GOES-HERE]}}}

(def temp1
  (o/synth "megasynth-osc"
    [freq 440]
    (let [saw (o/saw freq)]
      (o/out 0 saw))))


(o/defsynth test-env-gen6
  [freq 440
   amp 0.5
   attack 0.01
   decay 0.9
   sustain 0.0
   release 0.9
   gate 1.0]
  (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
        sig (* amp env (o/sin-osc freq))]
    (o/out 0 (o/pan2 sig))))

(test-env-gen6)

(let [sn0 (o/synth [freq 440
                    amp 0.5
                    attack 0.01
                    decay 0.9
                    sustain 1.0
                    release 0.9
                    gate 1.0]
            (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
                  sig (* amp env (o/sin-osc freq))]
              (o/out 0 (o/pan2 sig))))
      vc (sn0)]
  (Thread/sleep 500)
  (o/ctl vc :freq 660)
  (Thread/sleep 500)
  (o/ctl vc :gate 0.0))

(comment
  (let [sn0 (o/synth [freq 440
                      amp 0.5
                      attack 0.01
                      decay 0.9
                      sustain 1.0
                      release 0.9
                      gate 1.0]
                     (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
                           sig (* amp env (o/sin-osc freq))]
                       (o/out 0 (o/pan2 sig))))]
    (play sn0 600)
    (Thread/sleep 200)
    (play sn0 600 :freq 220)
    (Thread/sleep 200)
    (play sn0 600 :freq 660)
    (Thread/sleep 200)
    (play sn0 600 :freq 110)
    (Thread/sleep 200)
    (play sn0 600 :freq 880)
    (Thread/sleep 200))
  (comment))

#_
(o/stop)

(comment
  (let [sn0 (o/synth [freq 440
                      amp 0.5
                      attack 0.9
                      decay 2.0
                      sustain 0.0
                      release 2.0
                      gate 1.0]
                     (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
                           sig (* amp env (o/sin-osc freq))]
                       (o/out 0 (o/pan2 sig))))
        rng (apply concat (repeat 4 (range 1 10)))
        fs (map (fn [i] (* i 110)) rng)
        fs' (shuffle fs)]
    (reduce (fn [agg item]
              (play sn0 600 :freq item)
              (Thread/sleep 200)
              nil)
            nil
            fs'))
  (comment))

#_
(o/stop)


(defn ugen-semitone-ratio [semi]
  (o/pow 2.0 (o/mul-add semi (/ 1.0 12.0) 0)))

(o/defsynth fc-lead-342
  [freq 440
   amp 0.6
   detune 0.07
   pan-spread 0.4
   pitch-env-amt 0.0 ;; Set to >0 to activate pitch sweep
   amp-attack 0.01
   amp-decay 0.1
   amp-sustain 0.8
   amp-release 0.2
   rev-mix 0.3
   rev-room 0.5
   gate 1]
  (let [; Detuned saws
        f1 (* freq (ugen-semitone-ratio (* -1.0 detune)))
        f2 (* freq (ugen-semitone-ratio -0.03))
        f3 (* freq (ugen-semitone-ratio 0.03))
        f4 (* freq (ugen-semitone-ratio detune))
        saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
              (o/pan2 (o/saw f2) (* -0.3 pan-spread))
              (o/pan2 (o/saw f3) (* 0.3 pan-spread))
              (o/pan2 (o/saw f4) pan-spread)]
        mixed (o/mix saws)

        ; Pitch envelope (optional)
        pitch-env (o/env-gen (o/adsr 0.0 0.01 0.0 0.01) gate)
        pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
        pitched (* mixed pitch-mod)

        ; Amp envelope
        amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                           gate :action o/FREE)
        voiced (* pitched amp-env amp)

        ; Reverb
        wet (o/free-verb voiced rev-mix rev-room)]
    (o/out 0 wet)))

#_
(play fc-lead-342 400 :amp 2.0 :pitch-env-amt 18.0)

#_
(o/stop)

#_
(play final-countdown-claude0 440)

#_
(play final-countdown2 440)

#_
(play final-countdown2 440)



(o/defsynth kick-drum [amp 0.5]
  (let [env (o/env-gen (o/perc 0.01 0.3) :action o/FREE)
        freq-env (o/env-gen (o/envelope [60 30] [0.1]))
        osc (o/sin-osc freq-env)]
    (o/out 0 (* osc env amp))))

(o/defsynth final-countdown-claude0 [freq 440 amp 0.4 gate 1 cutoff 3000 res 0.3]
  (let [env (o/env-gen (o/adsr 0.02 0.3 0.7 0.5) gate :action o/FREE)
        ; Layer multiple oscillators for richness
        osc1 (o/saw freq)
        osc2 (o/saw (* freq 1.01)) ; Slight detune for width
        osc3 (o/pulse freq 0.3)    ; Add some pulse wave character
        
        ; mix the oscillators
        mixed (+ (* osc1 0.4) (* osc2 0.3) (* osc3 0.3))
        
        ; Filter with envelope modulation
        filter-env (o/env-gen (o/adsr 0.01 0.4 0.6 0.3) gate)
        filtered (o/rlpf mixed (+ cutoff (* filter-env 2000)) res)
        
        ; Add some bite with distortion
        distorted (o/tanh (* filtered 2))
        
        ; Final output with some stereo width
        left (* distorted env amp)
        right (* distorted env amp 0.9)]
    (o/out 0 [left right])))

#_
(play final-countdown-claude0 400)

#_
(o/defsynth final-countdown-lead [freq 440 amp 0.4 gate 1]
  (let [; Amp envelope - quick attack, full sustain, short release
        amp-env (o/env-gen #_(o/envelope [0 1 1 0] [0.01 0.5 0.08] :sustain 1)
                           (o/envelope [0 1 1 0] [0.01 0.5 0.08])
                           gate :action o/FREE)
        
        ; Pitch modulation envelope - 18 semitone drop over 10ms
        pitch-env (o/env-gen (o/envelope [18 0] [0.01])
                             :action o/NO-ACTION)
        modulated-freq (* freq (o/midi->hz pitch-env))
        
        ; 4-voice unison saw waves with 7-cent detune spread
        detune-cents 7
        detune-ratio (Math/pow 2 (/ detune-cents 1200))
        
        osc1 (o/saw modulated-freq)
        osc2 (o/saw (* modulated-freq detune-ratio))
        osc3 (o/saw (* modulated-freq (/ 1 detune-ratio)))
        osc4 (o/saw (* modulated-freq (* detune-ratio detune-ratio)))
        
        ; Mix unison voices with stereo spread
        mixed (+ osc1 osc2 osc3 osc4)
        
        ; Apply amp envelope
        shaped (* mixed amp-env amp)
        
        ; Add reverb (30% wet, medium size)
        reverbed (+ (* shaped 0.7) (* (o/free-verb shaped 0.5 0.5 0.5) 0.3))
        
        ; Stereo output with slight spread
        left reverbed
        right (* reverbed 0.9)]
    
    (o/out 0 [left right])))

(o/defsynth fc-lead-chatgpt0
  [freq 440
   amp 0.5
   gate 1
   cutoff 2500
   res 0.25
   detune 0.01
   pulse-width 0.15
   drive 2.0
   pan 0.0]
  (let [; Envelopes
        amp-env (o/env-gen (o/adsr 0.005 0.1 0.85 0.1) gate :action o/FREE)
        filter-env (o/env-gen (o/adsr 0.005 0.2 0.6 0.1) gate)
        
        ; Oscillators
        saw1 (o/saw freq)
        saw2 (o/saw (* freq (+ 1 detune)))
        saw3 (o/pulse freq pulse-width)
        mixed (o/mix [(* saw1 0.5)
                      (* saw2 0.3)
                      (* saw3 0.3)])
        
        ; Filter with envelope modulation
        filt (o/rlpf mixed (+ cutoff (* 2000 filter-env)) res)
        
        ; Distortion and panning
        driven (o/tanh (* filt drive))
        sig (* driven amp-env amp)]
    
    (o/out 0 (o/pan2 sig pan))))

#_
(play fc-lead-chatgpt0 400 :amp 2.0)

(defn ugen-semitone-ratio [semi]
  (o/pow 2.0 (o/mul-add semi (/ 1.0 12.0) 0)))

(o/defsynth fc-lead-501
  [freq 440
   amp 0.6
   detune 0.07
   pan-spread 0.4
   pitch-env-amt 18.0 ;; Set to >0 to activate pitch sweep
   amp-attack 0.01
   amp-decay 0.1
   amp-sustain 0.8
   amp-release 0.2
   rev-mix 0.3
   rev-room 0.5
   gate 1]
  (let [ ;; Pitch envelope (optional)
        pitch-env (o/env-gen (o/adsr 0.0 0.06 0.0 0.01) gate)
        pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
        freq' (* freq pitch-mod)
        ;; Detuned saws
        f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
        f2 (* freq' (ugen-semitone-ratio -0.03))
        f3 (* freq' (ugen-semitone-ratio 0.03))
        f4 (* freq' (ugen-semitone-ratio detune))
        saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
              (o/pan2 (o/saw f2) (* -0.3 pan-spread))
              (o/pan2 (o/saw f3) (* 0.3 pan-spread))
              (o/pan2 (o/saw f4) pan-spread)]
        mixed (o/mix saws)
        
        ;; Amp envelope
        amp-env (o/env-gen (o/adsr 0.1 amp-decay amp-sustain amp-release)
                           gate :action o/FREE)
        voiced (* mixed amp-env amp)

        ;; Reverb
        wet (o/free-verb voiced rev-mix rev-room)]
    (o/out 0 wet)))

(def fc-lead-x
  (o/synth 
      [freq 440
       amp 2
       detune 0.07
       pan-spread 0.4
       pitch-env-amt 9.0 ;; Set to >0 to activate pitch sweep
       amp-attack 0.05
       amp-decay 0.1
       amp-sustain 0.8
       amp-release 0.2
       rev-mix 0.3
       rev-room 0.6
       gate 1]
      (let [ ;; Pitch envelope (optional)
            pitch-env (o/env-gen (o/adsr 0.0 0.06 0.0 0.01) gate)
            pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
            freq' (* freq pitch-mod)
            ;; Detuned saws
            f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
            f2 (* freq' (ugen-semitone-ratio -0.03))
            f3 (* freq' (ugen-semitone-ratio 0.03))
            f4 (* freq' (ugen-semitone-ratio detune))
            saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
                  (o/pan2 (o/saw f2) (* -0.3 pan-spread))
                  (o/pan2 (o/saw f3) (* 0.3 pan-spread))
                  (o/pan2 (o/saw f4) pan-spread)]
            mixed (o/mix saws)
                      
            ;; Amp envelope
            amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                               gate :action o/FREE)
            voiced (* mixed amp-env amp)

            ;; Reverb
            wet (o/free-verb voiced rev-mix rev-room)]
        (o/out 0 wet))))

(def fc-lead-x2
  (o/synth 
      [freq 440
       amp 2
       detune 0.07
       pan-spread 0.4
       pitch-env-amt 9.0 ;; Set to >0 to activate pitch sweep
       amp-attack 0.05
       amp-decay 0.1
       amp-sustain 0.8
       amp-release 0.2
       rev-mix 0.3
       rev-room 0.6
       gate 1]
      (let [pitch-env (o/env-gen (o/adsr 0.0 0.025 0.0 0.01) gate)
            pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
            freq' (* freq pitch-mod)

            ;; Detuned saws
            f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
            f2 (* freq' (ugen-semitone-ratio -0.03))
            f3 (* freq' (ugen-semitone-ratio 0.03))
            f4 (* freq' (ugen-semitone-ratio detune))
            saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
                  (o/pan2 (o/saw f2) (* -0.3 pan-spread))
                  (o/pan2 (o/saw f3) (* 0.3 pan-spread))
                  (o/pan2 (o/saw f4) pan-spread)]
            mixed (o/mix saws)

            ;; Filter envelope
            filter-env (o/env-gen (o/adsr 0.01 0.1 0.0 0.1) gate)
            filtered (o/rlpf mixed
                             (+ 2000 (* 2000 filter-env)) ;; opens from 2000 to ~4000
                             0.1)

            driven (o/tanh (* filtered 3)) ;; distortion
            amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                               gate :action o/FREE)
            voiced (* driven amp-env amp)
            wet (o/free-verb voiced rev-mix rev-room)]
        (o/out 0 wet))))

(def fc-lead-x3
  (o/synth 
    [freq 440
     amp 1.5
     detune 0.07
     pan-spread 0.4
     pitch-env-amt 12.0 ;;9.0
     amp-attack 0.05
     amp-decay 0.1
     amp-sustain 0.8
     amp-release 0.2
     cutoff-base 4000 ;; 2000
     cutoff-env-amt 4000 ;; 2000
     res 0.1 ;; 0.1
     rev-mix 0.3
     rev-room 0.6
     gate 1]
    (let [;; Pitch sweep
          pitch-env (o/env-gen (o/adsr 0.0 0.06 0.0 0.01) gate)
          pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
          freq' (* freq pitch-mod)

          ;; Detuned saws
          f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
          f2 (* freq' (ugen-semitone-ratio -0.03))
          f3 (* freq' (ugen-semitone-ratio 0.03))
          f4 (* freq' (ugen-semitone-ratio detune))
          saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
                (o/pan2 (o/saw f2) (* -0.3 pan-spread))
                (o/pan2 (o/saw f3) (* 0.3 pan-spread))
                (o/pan2 (o/saw f4) pan-spread)]
          mixed (o/mix saws)

          ;; Filter for body fullness
          filter-env (o/env-gen (o/adsr 0.01 0.1 0.0 0.1) gate)
          cutoff (+ cutoff-base (* cutoff-env-amt filter-env))
          filtered (o/rlpf mixed cutoff res)

          ;; Light drive
          driven (o/tanh (* filtered 3))

          ;; Amp envelope
          amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                             gate :action o/FREE)
          voiced (* driven amp-env amp)

          ;; Reverb
          wet (o/free-verb voiced rev-mix rev-room)]
      (o/out 0 wet))))


(def fc-lead-x4
  (o/synth 
      [freq 440
       amp 2
       detune 0.07
       pan-spread 0.4
       pitch-env-amt 6.0 ;; 9.0
       amp-attack 0.05
       amp-decay 0.1
       amp-sustain 0.8
       amp-release 0.2
       rev-mix 0.3
       rev-room 2.0 ;; 0.6
       pulse-width 0.3
       bpf-freq 1800 ;; 1400
       bpf-q 2.0     ;; 1.0
       dist-gain 4.0
       gate 1]
      (let [ ;; Pitch envelope
            pitch-env (o/env-gen (o/adsr 0.0 0.05 0.0 0.01) gate)
            pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
            freq' (* freq pitch-mod)

            ;; Oscillators: saw + pulse blend
            f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
            f2 (* freq' (ugen-semitone-ratio detune))
            f3 freq'
            saw (o/saw f3)
            pulse1 (o/pulse f1 pulse-width)
            pulse2 (o/pulse f2 pulse-width)

            ;; Pan and mix
            sigs [(o/pan2 saw 0.0)
                  (o/pan2 pulse1 (* -0.4 pan-spread))
                  (o/pan2 pulse2 (* 0.4 pan-spread))]
            mixed (o/mix sigs)

            ;; Bandpass filtering (trumpet-like tone focus)
            brassy (o/bpf mixed bpf-freq bpf-q)

            ;; Distortion after BPF for harmonic "bite"
            driven (o/tanh (* brassy dist-gain))

            ;; Amp envelope
            amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                               gate :action o/NO-ACTION)
            tail-env (o/env-gen (o/lin 0.001 1.0 1.0)
                                gate :action o/FREE) 
            voiced (* driven amp-env amp tail-env)

            ;; Reverb
            wet (o/free-verb voiced rev-mix rev-room)]
        (o/out 0 wet))))

(comment)


(comment
  (let [pea 5.0]
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 200)
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 493)
    (Thread/sleep 200)
    (play fc-lead-501 500 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 500)
    (play fc-lead-501 1000 :amp 2.0 :pitch-env-amt pea :freq 369)
    (Thread/sleep 1500)
    
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 587)
    (Thread/sleep 200)
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 200)
    (play fc-lead-501 400 :amp 2.0 :pitch-env-amt pea :freq 587)
    (Thread/sleep 400)
    (play fc-lead-501 400 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 400)
    (play fc-lead-501 1000 :amp 2.0 :pitch-env-amt pea :freq 493))
  
  (comment))

#_
(play fc-lead-chatgpt0 440)

(o/defsynth detune-test
  [freq 440 amp 0.7 detune 0.07 gate 1]
  (let [;;f1 (* freq (ugen-semitone-ratio (* -1.0 detune)))
        f2 freq
        f3 (* freq (ugen-semitone-ratio detune))
        saws [#_(o/pan2 (o/saw f1))
              (o/pan2 (o/saw f2))
              (o/pan2 (o/saw f3))]
        mixed (o/mix saws)
        env (o/env-gen (o/adsr 0.01 0.3 1 0.5) :gate gate :action o/FREE)]
    (o/out 0 (* env mixed))))

#_
(play detune-test 500 :detune 0.07)

#_
(play  (o/synth [] (o/out 0 (o/saw 440))) 500)

#_
(o/stop)

#_
(o/kill @*da-funk0-voice)

#_
(test-env-gen3)

#_
(o/demo (temp1 660))

#_
(o/stop)

(defmulti mk-synth (fn [s-type _] s-type))


(defn produce-synth [s-type args]
  (let [k [s-type args]]
    (if-let [r (@synth-cache& k)]
      r
      (let [r (mk-synth s-type args)]
        (swap! synth-cache& assoc k r)
        r))))


;; =============================================================================
;; START - Graph Compilation
;; =============================================================================

#_
(defn compile-patch-def!
  [patch-name-kw patch-def]
  (let [patch-state (get @patch-states& patch-name-kw {})]
    (loop [[head & tail] (keys patch-def)
           ps patch-state]
      (let [vtail (vec tail)]
        (cond
          (nil? head) ps
          
          (= (-> head patch-state :k) (patch-def head))
          (recur vtail
                 (rewire-buses head ps))

          :else (if-let [missing (-> head
                                     (find-missing-reqs patch-def patch-state)
                                     not-empty)]
                  (recur (into missing vtail)
                         ps)
                  (recur vtail
                         (update-patch-state head patch-def patch-state))))))))


;; =============================================================================
;; END - Graph Compilation
;; =============================================================================

(def patch0
  {:mix0.b {:ty :knob
            :default 0.5
            :range [0.0 1.0]}
   :lfo0.freq {:ty :knob
               :default 0.3
               :range [0.1 10.0]}
   :lfo0.amp {:ty :knob
              :default 0.4
              :range [0.0 1.0]}
   :lfo0 {:ty :lfo
          :inputs {:freq :k1 :amp :k2}}
   :mix0 {:ty :mixer
          :inputs {:a :lfo0 :b :k0}}
   :osc0 {:ty :oscillator
          :inputs {:amp :mix0}}
   :out  {:ty :output
          :inputs {:in :osc0}}})

(def synth-graph0
  {:lfo0 {:ty :lfo
          :inputs {:freq :k1 :amp :k2}}
   :mix0 {:ty :mixer
          :inputs {:a :lfo0 :b :k0}}
   :osc0 {:ty :oscillator
          :inputs {:amp :mix0}}
   :out  {:ty :output
          :inputs {:in :osc0}}})

(def knobs0
  {:mix0.b {:default 0.5
            :range [0.0 1.0]}
   :lfo0.freq {:default 0.3
               :range [0.1 10.0]}
   :lfo0.amp {:default 0.4
              :range [0.0 1.0]}})

#_
(def m-out0 (midi/midi-out))

#_
(def m-in0 nil)



#_
(def m-in0 (midi/midi-in "O"))

#_
(clojure.pprint/pprint m-in0)

(defn mk-receiver []
  (proxy [Receiver] []
    (close [] nil)
    (send [msg timestamp] (cond (instance? ShortMessage msg )
                                (@midi-msg-fn&
                                 (assoc (midi/midi-msg msg timestamp)
                                        :device :DUMMY))

                                (instance? SysexMessage msg)
                                (@midi-msg-fn&
                                 {:timestamp timestamp
                                  :data (.getData ^SysexMessage msg)
                                  :status (.getStatus ^SysexMessage msg)
                                  :length (.getLength ^SysexMessage msg)
                                  :device :DUMMY})))))


(defn setup-midi-in []
  (let [m-in (midi/midi-in "O")]
    (if-not (nil? m-in)
      (let [_ (reset! current-midi& m-in)
            rcvr (mk-receiver)
            route (midi/midi-route m-in
                                   {:receiver rcvr})]
        true)
      (throw (Exception. "MIDI INPUT NOT FOUND")))))

#_(setup-midi-in)

#_
(def rcvr1 (mk-receiver))

#_
(reset! midi-msg-fn& midi-handler)

#_
(reset! current-synth& da-funk-mono)


(defn setup-all []
  (reset! midi-msg-fn& midi-handler)
  (reset! current-synth& da-funk-mono)
  (setup-midi-in))

#_(setup-all)

(comment

  (set-synth! da-funk-mono)

  (def fc-lead-501-mono (->mono fc-lead-x))

  (do (def fc-lead-501-mono (->mono fc-lead-x))
      (set-synth! fc-lead-501-mono))

  (do (def fc-lead-501-mono (->mono fc-lead-x2))
      (set-synth! fc-lead-501-mono))

  (do (def fc-lead-501-mono (->mono fc-lead-x3))
      (set-synth! fc-lead-501-mono))

  (do (def fc-lead-501-mono (->mono fc-lead-x4))
      (set-synth! fc-lead-501-mono))

  (play fc-lead-x4 400)

  (comment))

#_(set-synth! fc-lead-501-mono)

#_(set-synth! da-funk-mono0)

