(ns megasynth.synths.final-countdown
  (:require [megasynth.common :as com]
            [overtone.live :as o]))


(swap! com/synths-ifaces&
       assoc :final-countdown
       (com/mk-synth-iface
        [amp [0.01 2 4.0]
         detune [0.0 0.07 0.2]
         pan-spread [0.0 0.4 1.0]
         pitch-env-amt [0.0 6.0 12.0] ;; 9.0
         amp-attack [0.0 0.05 0.2]
         amp-decay [0.0 0.1 0.2]
         amp-sustain [0.0 0.8 1.6]
         amp-release [0.0 0.2 0.4]
         rev-mix [0.0 0.3 0.6]
         rev-room [0.0 2.0 4.0] ;; 0.6
         pulse-width [0.0 0.3 0.6]
         bpf-freq [100 1800 4000] ;; 1400
         bpf-q [0.1 2.0 4.0]      ;; 1.0
         dist-gain [0.0 4.0 8.0]]
        (let [ ;; Pitch envelope
              pitch-env (o/env-gen (o/adsr 0.0 0.05 0.0 0.01) gate)
              pitch-mod (com/ugen-semitone-ratio (* pitch-env pitch-env-amt))
              freq' (* freq pitch-mod)

              ;; Oscillators: saw + pulse blend
              f1 (* freq' (com/ugen-semitone-ratio (* -1.0 detune)))
              f2 (* freq' (com/ugen-semitone-ratio detune))
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
          (o/out 0 wet))
        {:mono? true}))
