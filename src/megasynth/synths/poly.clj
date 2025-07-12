(ns megasynth.synths.poly
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
       assoc :poly
       (com/mk-synth-iface
           [amp [0.01 1.5 3.0]
            o1 [1.0 1.0 24.0]
            o2 [1.0 1.0 24.0]
            o3 [1.0 1.0 24.0]
            o4 [1.0 1.0 24.0]
            o5 [1.0 1.0 24.0]
            o6 [1.0 1.0 24.0]
            o7 [1.0 1.0 24.0]
            amp-attack [0.0 0.0 0.5]  ;; instant
            amp-decay [0.0 0.0 2.0] ;; very long decay
            amp-sustain [0.0 1.0 1.0]
            amp-release [0.0 0.0 1.0]]
           (let [ov [o1 o2 o3 o4 o5 o6 o7]
                 oscs (map #(o/saw (* freq (com/ugen-semitone-ratio %))) ov)
                 mixed (o/mix (vec oscs))

                 ;; Amp envelope
                 amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                                    gate :action o/FREE)
                 voiced (* amp mixed amp-env)]

             (o/out 0 voiced))

         {:mono? false}))
