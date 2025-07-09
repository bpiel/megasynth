(ns megasynth.synths.derezzed
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
       assoc :derezzed
       (com/mk-synth-iface
           [amp [0.01 0.9 2.0]
            sub-level [0.0 0.2 1.0]
            osc1-level [0.0 0.4 1.0]
            osc2-level [0.0 0.6 1.0]
            noise-level [0.0 0.2 1.0]
            detune [0.0 0.01 0.03]
            spread [0.0 0.5 1.0]
            pitch1-offset [0.0 0.03 0.1]   ; in semitones
            pitch2-offset [11.8 12.0 12.2] ; +1 octave detuned
            filter-cutoff [100 3000 8000]
            filter-res [0.0 0.65 1.0]
            keytrack [0.0 0.5 1.0]
            drive [1.0 2.0 4.0]
            delay-mix [0.0 0.35 0.6]
            delay-time [0.01 0.03 0.1]
            delay-fb [0.0 0.0 0.4]
            phaser-mix [0.0 0.5 1.0]
            phaser-fb [0.0 0.6 1.0]
            release [0.01 0.02 0.1]
            pan [0.0 0.0 0.0]]
           (let [freq1 (* freq (com/ugen-semitone-ratio pitch1-offset))
                 freq2 (* freq (com/ugen-semitone-ratio pitch2-offset))
                 sub-freq (/ freq 2)

                 osc1 (o/saw freq1)
                 osc2 (o/saw freq2)
                 sub (o/pulse sub-freq 0.5)
                 noise (o/white-noise)

                 mixed (+ (* osc1 osc1-level)
                          (* osc2 osc2-level)
                          (* sub sub-level)
                          (* noise noise-level))

                 detuned [(o/pan2 mixed (* -0.5 spread))
                          (o/pan2 mixed (* 0.5 spread))]

                 summed (o/mix detuned)
                 filtered (o/rlpf summed
                                  (+ filter-cutoff (* freq keytrack))
                                  filter-res)
                 driven (/ (o/tanh (* filtered drive)) drive)

                 ;; Effects
                 phased (o/allpass-c driven 0.01 phaser-fb)
                 phased-mix (o/x-fade2 driven phased phaser-mix 1.0)

                 delayed (o/comb-l phased-mix delay-time delay-time delay-fb)
                 delayed-mix (o/x-fade2 phased-mix delayed delay-mix 1.0)

                 env (o/env-gen (o/adsr 0.0 0.01 1.0 release) gate :action o/FREE)
                 out (* delayed-mix env amp)]
             (o/out 0 out))
           {:mono? true
            :legato? true}))
