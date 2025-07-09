(ns megasynth.synths.jump
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
       assoc :jump
       (com/mk-synth-iface
           [amp [0.01 1.5 3.0]
            detune [0.0 0.09 0.2] ;; 9 cents detune
            
            ;; Filter
            cutoff [200 8000 12000] ;; ~85% of Nyquist
            res [0.1 0.25 0.5]
            
            ;; Reverb
            rev-mix [0.0 0.3 0.6]
            rev-room [0.1 1.5 3.0]

            ;; Amp envelope
            amp-attack [0.0 0.0 0.1]  ;; instant
            amp-decay [0.1 10.0 12.0] ;; very long decay
            amp-sustain [0.0 0.0 0.1] ;; no sustain
            ;; amp-sustain [0.0 1.0 1.0] ;; no sustain
            amp-release [0.01 0.08 0.3]

            pan-spread [0.0 0.4 1.0]]

           (let [ ;; Oscillator detuning
                 ratios [(* -1.0 detune)
                         (* -0.5 detune)
                         0.0
                         (* 0.5 detune)
                         detune]

                 oscs (map #(o/saw (* freq (com/ugen-semitone-ratio %))) ratios)
                 panned (map-indexed
                         (fn [i s] (o/pan2 s (* pan-spread (- (/ i 2) 1)))) ; spread voices
                         oscs)
                 mixed (o/mix (vec panned))

                 ;; Filter
                 filtered (o/rlpf mixed cutoff res)

                 ;; Amp envelope
                 amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                                    gate :action o/NO-ACTION)

                 tail-env (o/env-gen (o/lin 0.001 1.0 5.0)
                                     gate :action o/FREE)

                 voiced (* filtered amp-env amp tail-env)

                 ;; Reverb
                 wet (o/free-verb voiced rev-mix rev-room)]

             (o/out 0 wet))

         {:mono? false})) ;; Polyphonic
