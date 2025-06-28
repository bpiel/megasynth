(ns megasynth.bill-v1
  (:require [overtone.live :as o]
            [overtone.midi :as midi])
  (:import [javax.sound.midi MidiDevice MidiDevice$Info MidiSystem
            Receiver Sequencer ShortMessage Synthesizer SysexMessage Transmitter]))

(def synth-cache& (atom {}))
(def patch-states& (atom {}))

(def midi-msg-fn& (atom identity))

(def notes& (atom {}))

(defn midi->hz [note]
  (* 440 (Math/pow 2 (/ (- note 69) 12.0))))


;; for a given synth
;; maintain an instance per key press
;; set gate to 0 on note-off
;; clean up dead instances


#_
(reset! midi-msg-fn&
        (fn [e]
          (def e1 e)
          (clojure.pprint/pprint e)))

#_
(reset! midi-msg-fn& midi-handler)

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

(play default-synth2 400)

(defn note-on! [note-id]
  (try
    (let [freq (midi->hz note-id)
          node1 (da-funk-mono :freq freq)]
      (swap! notes& assoc note-id node1)
      node1)
    (catch Throwable e
      (clojure.pprint/pprint e))))

(defn note-off! [note-id]
  (when-let [node0 (@notes& note-id)]
    (when-not (-> node0 :status deref (= :destroyed))
      (o/ctl node0 :gate 0.0)))
  true)

(defn midi-handler [{:keys [command note] :as msg}]
  #_(clojure.pprint/pprint msg)
  (case command
    :note-on (note-on! note)
    :note-off (note-off! note)
    (clojure.pprint/pprint msg)))

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

(defn play [synth dur-ms & args]
  (let [vc (apply synth args)]
    (future
      (Thread/sleep dur-ms)
      (o/ctl vc :gate 0.0))
    true))

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

(o/stop)

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

(o/stop)

(o/defsynth env-osc
  [out-bus 0
   freq 440
   gate 1
   atk 0.01
   dec 0.2
   sus 0.6
   rel 0.3]
  (let [env (o/env-gen
              (o/env-adsr atk dec sus rel)
              gate 1 0 1
              :action :free)
        osc (o/sin-osc freq)
        sig (* osc env)]
    (o/out out-bus sig)))

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

(do
  (da-funk-mono 220)
  (Thread/sleep 300)
  (da-funk-mono 260))

(o/defsynth final-countdown0
  [freq 440
   dur 0.5
   amp 0.8
   detune 0.03
   cutoff 2000
   res 0.3
   pan 0.0
   boost 2.0]
  (let [env (o/env-gen (o/adsr 0.01 0.1 0.8 0.1) (o/line:kr 1.0 0.0 dur) :action o/FREE)
        detune-freqs [freq (* freq (- 1 detune)) (* freq (+ 1 detune))]
        osc (o/mix [(o/saw freq)
                    (o/saw (* freq (- 1 detune)))
                    (o/saw (* freq (+ 1 detune)))])
        #_(apply o/mix (mapv o/saw detune-freqs))
        filtered (o/rlpf osc cutoff res)
        sig (-> filtered
                (* env amp)
                o/pan2
                (* boost))]
    (o/out 0 sig)))

(o/defsynth final-countdown1
  [freq 440
   dur 0.4
   amp 0.8
   detune 0.02
   cutoff 3500
   res 0.2
   pan 0.0
   boost 2.5
   fifth 0.4     ;; blend in a 5th above
   dist 0.01]    ;; distortion/clipping level
  (let [env (o/env-gen (o/adsr 0.01 0.05 0.9 0.05) (o/line:kr 1.0 0.0 dur) :action o/FREE)
        saw1 (o/saw freq)
        saw2 (o/saw (* freq (- 1 detune)))
        saw3 (o/saw (* freq (+ detune)))
        saw5 (o/saw (* freq (Math/pow 2 (/ 7.0 12.0)))) ;; perfect 5th
        osc (o/mix [(* saw1 0.9)
                    (* saw2 0.6)
                    (* saw3 0.6)
                    (* saw5 fifth)])
        filtered (o/rlpf osc cutoff res)
        distorted (-> filtered
                      (* env amp)
                      o/pan2
                      (o/clip2 dist)
                      (* boost))]
    (o/out 0 distorted)))

(o/defsynth final-countdown2
  [freq 440
   dur 0.5
   amp 1.0
   detune 0.015
   cutoff 4000
   res 0.2
   pan 0.0
   boost 1.5
   dist-level 0.02]
  (let [env (o/env-gen (o/adsr 0.01 0.08 0.8 0.05)
                       (o/line:kr 1.0 0.0 dur)
                       :action o/FREE)
        saw1 (o/saw freq)
        saw2 (o/saw (* freq (- 1 detune)))
        saw3 (o/saw (* freq (+ detune)))
        osc (o/mix [saw1 saw2 saw3])
        filtered (o/rlpf osc cutoff res)
        sig (-> filtered
                (* env amp)
                o/pan2
                (o/clip2 dist-level)
                (* boost))]
    (o/out 0 sig)))


(play final-countdown2 440)

(play-final-countdown 64 :dur 0.4 :amp 1.2 :fifth 0.4 :cutoff 3500 :boost 2.5)


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
        
        ; Mix the oscillators
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


(play fc-lead-chatgpt0 440)

(o/kill @*da-funk0-voice)

(test-env-gen3)

#_
(o/demo (temp1 660))

(hash (temp1 660))

(o/stop)

o/env-gen

o/FREE
o/on-event
o/node-live?

o/on-node-destroyed

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

(def m-out0 (midi/midi-out))

(def m-in0 nil)
(def m-in0 (midi/midi-in "O"))


(clojure.pprint/pprint m-in0)

(def oe0 (o/on-event [:midi]
                     (fn [e]
                       (def e0 e)
                       (clojure.pprint/pprint e))
                     ::catch-all))

(o/remove-event-handler ::catch-all)

(o/midi-note-on)

(defn mk-receiver [short-msg-fn sysex-msg-fn]
  (proxy [Receiver] []
    (close [] nil)
    (send [msg timestamp] (cond (instance? ShortMessage msg )
                                (short-msg-fn
                                 (assoc (midi/midi-msg msg timestamp)
                                        :device :DUMMY))

                                (instance? SysexMessage msg)
                                (sysex-msg-fn
                                 {:timestamp timestamp
                                  :data (.getData ^SysexMessage msg)
                                  :status (.getStatus ^SysexMessage msg)
                                  :length (.getLength ^SysexMessage msg)
                                  :device :DUMMY})))))

(defn mk-receiver2 []
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

(def rcvr0 (mk-receiver (fn [e]
                          (def e1 e)
                          (println "RECEIVED")
                          (clojure.pprint/pprint e))
                        nil))

(def rcvr1 (mk-receiver2))

(def route0 (midi/midi-route m-in0
                             {:receiver rcvr1}))

(let [msg-fn (fn [x]
               (clojure.pprint/pprint x))
      rcvr (mk-receiver msg-fn nil)]
  (midi/midi-note-on {:receiver rcvr}
                     40 100))

ShortMessage/
