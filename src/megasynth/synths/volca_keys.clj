(ns megasynth.synths.volca-keys
  (:require [megasynth.common :as com]
            [overtone.live :as o]))



(swap! com/synths-ifaces&
       assoc :volca-keys0
       (com/mk-synth-iface
           [amp [0.0 0.9 2.0]
            detune [0.0 0.0 1.0]

            ;; Filter
            cutoff [0.0 1200 4000]
            res [0.0 0.7 2.0]
            env-amt [0 1000 2000]

            ;; Envelopes
            attack [0.0 0.01 1.0]
            decay [0.0 0.2 3.0]
            sustain [0.0 0.5 3.0]
            release [0.0 0.3 3.0]

            ;; LFO
            lfo-rate [0.1 5.0 10.0]    ;; Hz
            lfo-depth [0.0 1.0 3.0]   ;; global depth scaler
            lfo->pitch [0.0 0.0 1.0]  ;; amount in semitones
            lfo->cutoff [0.0 0.0 1.0] ;; amount added to cutoff
            lfo->amp [0.0 0.0 1.0]    ;; amplitude modulation depth

                        ;; Delay
            delay-time [0.0 0.0 2.0]
            delay-fb [0.0 0.4 2.0]
            delay-mix [0.0 0.3 1.0]
            
            pan [-1.0 0.0 1.0]]
           (let [ ;; LFO signal (triangle wave)
                 lfo (o/lf-tri lfo-rate)

                 ;; Apply LFO modulations
                 pitch-lfo (* lfo (com/ugen-semitone-ratio-1 (* lfo->pitch lfo-depth)))
                 cutoff-lfo (* lfo lfo->cutoff lfo-depth cutoff)
                 amp-lfo ;;(+ 1.0 (* lfo lfo->amp lfo-depth))  ;; centered around 1.0
                 (o/lin-lin lfo -1 1 (- 1 lfo->amp) (+ 1 lfo->amp))


                 ;; Detuned 3-voice oscillator
                 freq*lfo (+ freq (* freq pitch-lfo))
                 ratios [(* -1.0 detune) 0 detune]
                 saws (mapv #(o/saw (* freq*lfo (com/ugen-semitone-ratio %))) ratios)
                 osc (o/mix saws)

                 ;; Envelopes
                 amp-env (o/env-gen (o/adsr attack decay sustain release)
                                    gate :action o/FREE)
                 mod-cutoff (+ cutoff cutoff-lfo (* env-amt amp-env))

                 ;; Filtered signal
                 filtered (o/rlpf osc mod-cutoff res)

                 ;; Amplitude envelope w/ tremolo
                 voiced (* filtered amp-env amp amp-lfo)

                 ;; Delay
                 delayed (o/comb-l voiced delay-time delay-time delay-fb)
                 mixed (o/x-fade2 voiced delayed delay-mix 1.0)
                 
                 ;; Output
                 out (o/pan2 mixed pan)]
             (o/out 0 out))))
