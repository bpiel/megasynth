(ns megasynth.synths.multiple
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

#_
(swap! com/synths-ifaces&
       assoc :multiple
       (com/mk-synth-iface
           [amp [0.01 1.5 3.0]
            o1 [0.0 1.0 1.0]
            o2 [0.0 0.0 1.0]
            o3 [0.0 0.0 1.0]
            o4 [0.0 0.0 1.0]
            amp-attack [0.0 0.0 0.5]  ;; instant
            amp-decay [0.0 0.0 2.0] ;; very long decay
            amp-sustain [0.0 1.0 1.0]
            amp-release [0.0 0.0 1.0]]
           (let [ov [o1 o2 o3
                     (o/mul-add o2 0.5 0.0)
                     (o/mul-add o3 0.5 0.0)
                     (o/mul-add o2 0.25 0.0)
                     (o/mul-add o3 0.25 0.0)]
                 oscs0 (map
                        #(* % (o/sin-osc (* freq (o/pow 2 %2))))
                        ov
                        (range 0.0 10.0))
                 mixed (o/mix (vec oscs0))

                 ;; Amp envelope
                 amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                                    gate :action o/FREE)
                 voiced (* amp mixed amp-env)]

             (o/out 0 voiced))

         {:mono? false}))


(swap! com/synths-ifaces&
       assoc :multiple
       (com/mk-synth-iface
         [freq [30.0 100.0 440.0]
          amp [0.01 1.0 0.4]
          even [0.0 1.0 0.5]
          odd [0.0 1.0 0.5]
          noise-amt [0.0 1.0 0.3]
          decay [0.05 2.0 0.6]
          filter-brightness [200.0 8000.0 2000.0]]
         
         (let [;;gate 1

               ;; Harmonic shaping envelopes
               env (o/env-gen (o/perc 0.005 decay) gate :action o/FREE)
               decay-curve (map (fn [mult]
                                  (* (o/env-gen (o/perc 0.005 (* decay (/ 1.0 mult))) gate)
                                     (o/sin-osc (* freq mult))))
                                [1 2 3 4 5 6 7 8])
               
               ;; Separate even and odd partials
               evens (->> decay-curve (filter #(even? (+ 1 (.indexOf decay-curve %)))) (apply +))
               odds  (->> decay-curve (filter #(odd?  (+ 1 (.indexOf decay-curve %)))) (apply +))
               
               harmonic-mix (o/mix [(o/mul-add even evens 0.0)
                                    (o/mul-add odd odds 0.0)])
               
               ;; Transient noise burst
               noise-env (o/env-gen (o/perc 0.001 0.02) gate)
               noise (* noise-amt noise-env (o/white-noise))
               
               ;; Resonant filter sweep to simulate body resonance
               cutoff (+ 100 (o/mul-add filter-brightness env 0.0))
               filtered (o/rlpf (+ harmonic-mix noise) cutoff 0.8)
               
               sig (* amp filtered env)]
           
           (o/out 0 sig))

         {:mono? false}))

#_(o/stop)
