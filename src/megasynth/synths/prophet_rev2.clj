(ns megasynth.synths.prophet-rev2
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
       assoc :prophet-rev2
       (com/mk-synth-iface
        [;; 1
         amp            [0.01 1.0 3.0]

         ;; 2-4 oscillators
         detune         [0.0 0.08 0.2]
         osc-mix        [0.0 0.5 1.0]
         pulse-width    [0.05 0.5 0.95]

         ;; 5-7 filter
         cutoff         [60 2200 12000]
         res            [0.05 0.2 0.85]
         filt-env-amt   [0.0 0.8 2.0]

         ;; 8-10 envelopes
         env-attack     [0.0 0.01 1.5]
         env-decay      [0.01 0.3 5.0]
         env-release    [0.01 0.4 8.0]

         ;; 11-12 modulation
         lfo-rate       [0.05 4.5 20.0]
         lfo-amt        [0.0 0.08 0.7]]

        (let [;; --- LFO ---
              lfo (o/sin-osc lfo-rate)
              vib-ratio (com/ugen-semitone-ratio (* lfo lfo-amt))

              ;; --- Oscillators ---
              ;; Osc 1 is centered, osc 2 is detuned and both receive vibrato.
              f1 (* freq vib-ratio)
              f2 (* freq
                    vib-ratio
                    (com/ugen-semitone-ratio detune))

              osc1-saw   (o/saw f1)
              osc1-pulse (o/pulse f1 pulse-width)
              osc1       (+ (* 0.65 osc1-saw)
                            (* 0.35 osc1-pulse))

              osc2-saw   (o/saw f2)
              osc2-pulse (o/pulse f2 pulse-width)
              osc2       (+ (* 0.65 osc2-saw)
                            (* 0.35 osc2-pulse))

              ;; single knob crossfade between osc1 and osc2
              mixed (+ (* osc1 (- 1.0 osc-mix))
                       (* osc2 osc-mix))

              ;; --- Envelopes ---
              ;; Filter envelope is snappier / more synth-brassy.
              filt-env (o/env-gen
                        (o/adsr env-attack env-decay 0.0 env-release)
                        gate)

              amp-env  (o/env-gen
                        (o/adsr env-attack env-decay 0.75 env-release)
                        gate
                        :action o/NO-ACTION)

              tail-env (o/env-gen
                        (o/lin 0.001 1.0 5.0)
                        gate
                        :action o/FREE)

              ;; --- Filter ---
              cutoff'  (o/clip
                        (+ cutoff
                           (* cutoff filt-env-amt filt-env))
                        40 18000)

              filtered (o/rlpf mixed cutoff' res)

              ;; --- Output ---
              voiced   (* filtered amp-env amp tail-env)
              wet      (o/free-verb voiced 0.18 1.4)]

          (o/out 0 (o/pan2 wet 0.0)))

        {:mono? false}))
