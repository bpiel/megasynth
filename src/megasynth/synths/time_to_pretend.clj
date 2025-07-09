(ns megasynth.synths.time-to-pretend
  (:require [megasynth.common :as com]
            [overtone.live :as o]))


(com/mk-synth-iface
  [amp [0.01 1.0 2.0]
   pulse-width [0.01 0.3 0.99]
   filter-cutoff [100 6000 12000]
   filter-res [0.0 0.45 1.0]
   lfo-rate [2.0 8.0 20.0] ; Roughly equivalent to 1/32 note rate at 120 BPM
   lfo-depth [0.0 0.7 2.0] ; in semitones
   drive [1.0 1.1 2.0]
   chorus-mix [0.0 0.2 0.5]
   chorus-rate [0.1 0.3 0.5]
   rev-mix [0.0 0.1 0.3]
   rev-room [0.0 0.6 2.0]
   release [0.01 0.06 0.2]
   portamento [0.0 0.02 0.1]
   pan [0.0 0.0 0.0]]
  (let [freq-glide (o/lag freq portamento)
        ;; LFO for pitch modulation
        lfo (o/lf-tri lfo-rate)
        pitch-mod (com/ugen-semitone-ratio (* lfo lfo-depth))
        mod-freq (* freq-glide pitch-mod)

        ;; Pulse oscillator
        osc (o/pulse mod-freq pulse-width)

        ;; Filtering
        filtered (o/rlpf osc filter-cutoff filter-res)

        ;; Drive
        driven (/ (o/tanh (* filtered drive)) drive)

        ;; Chorus (basic implementation via modulated delay)
        mod-delay (o/mod-delay driven 0.015 chorus-rate)
        chorused (o/x-fade2 driven mod-delay chorus-mix 1.0)

        ;; Reverb
        reverbed (o/free-verb chorused rev-mix rev-room)

        ;; Amp envelope
        env (o/env-gen (o/adsr 0.0 0.01 1.0 release) gate :action o/FREE)
        out (* reverbed env amp)]
    (o/out 0 out))
  {:mono? true
   :legato? true})
