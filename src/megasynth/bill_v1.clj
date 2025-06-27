(ns megasynth.bill-v1
  (:require [overtone.live :as o]
            [overtone.midi :as midi])
  (:import [javax.sound.midi MidiDevice MidiDevice$Info MidiSystem
            Receiver Sequencer ShortMessage Synthesizer SysexMessage Transmitter]))

(def synth-cache& (atom {}))
(def patch-states& (atom {}))

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

(let [msg-fn (fn [x]
               (clojure.pprint/pprint x))
      rcvr (mk-receiver msg-fn nil)]
  (midi/midi-note-on {:receiver rcvr}
                     40 100))

ShortMessage/
