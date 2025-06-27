(ns megasynth.bill-v1
  (:require [overtone.live :as o]))

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


































