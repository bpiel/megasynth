(ns megasynth.synths.alpha1
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
       assoc :alpha1
       (com/mk-synth-iface
        [amp [0.01 1.5 5.0]
         sustain [0.01 0.08 1.0]
         release [0.01 0.08 1.0]
         detune [0.0 0.08 0.2]
         ;;pulse [0.0 0.02 1.0]
         cutoff [60 2200 12000]
         ;;res [0.0 0.4 0.95]
         ;;filter-env [0.0 0.8 3.0]
         lfo-to-filter [0.0 0.0 3.0]
         lfo-rate [0.1 4.0 20.0]
         ;;drive [1.0 1.5 6.0]
         delay-time [0.01 0.3 #_0.8 1]
         ;;delay-fb [0.0 0.0 0.85]
         delay-mix [-1.0 -0.9 1.0]
         reverb [0.0 0.3 1.0]]
        (let [lfo (o/lf-tri lfo-rate)

              ratios [(* -1.0 detune)
                      (* -0.5 detune)
                      0.0
                      (* 0.5 detune)
                      detune]
              oscs0 (map #(o/saw (* freq (com/ugen-semitone-ratio %))) ratios)
              oscs1 (map #(o/square (* freq (com/ugen-semitone-ratio %)))
                         ratios)
              #_oscs2 #_(map #(* pulse
                               (o/pulse (* freq (com/ugen-semitone-ratio %))))
                           ratios)              
              mixed (o/mix (vec (concat oscs0 oscs1 #_oscs2)))

              f-env (o/env-gen (o/adsr 0.0 0.15 0.0 0.1) gate)
              raw-cutoff (+ cutoff
                            #_(* filter-env f-env cutoff)
                            (* (o/absdif lfo 0) lfo-to-filter cutoff 0.5))
              mod-cutoff (o/select:ar (> raw-cutoff 18000)
                                      [raw-cutoff 18000])
              mod-cutoff (o/select:ar (< mod-cutoff 40)
                                      [mod-cutoff 40])
              filtered (o/rlpf mixed mod-cutoff 0.4 #_res)

              amp-env (o/env-gen (o/adsr 0.0 0.1 #_0.1 sustain #_0.7 release) gate :action o/FREE)
              voiced (* filtered amp-env)

              delayed (o/comb-l voiced 2.0 delay-time 0.5 #_delay-fb)
              delay-mixed (o/x-fade2 voiced delayed delay-mix 1.0)

              wet (o/free-verb delay-mixed reverb 1.0 0.0)]

          (o/out 0 (* wet amp) #_(o/pan2 wet 0.0)))
        {:mono? false
         :legato? true}))






































