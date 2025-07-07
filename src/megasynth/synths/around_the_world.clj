(ns megasynth.synths.around-the-world
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
       assoc :atw-lead-1
       (com/mk-synth-iface
           [ ;; Amp
            amp [0.0 0.8 2.0]
            pan [-1.0 0.0 1.0]

            ;; Detune and pulse width
            detune [0.0 0.02 0.2]
            pw1 [0.0 0.39 0.8] ;; ~9:30 clock (range 0.0 to 1.0)
            pw2 [0.0 0.5 1.0]  ;; ~12:05 clock

            ;; Filter
            cutoff [0.0 400 4000]
            res [0.0 0.5 1.0]
            env-amt [0.0 800 2000]
            svf-mix [0.0 0.7 2.0] ;; blend between osc and filtered

            ;; Filter envelope
            f-env-att [0.0 0.0 1.0]
            f-env-dec [0.0 0.02 0.2]
            f-env-sus [0.0 0.0 1.0]
            f-env-rel [0.0 0.0 1.0]

            ;; Amp envelope
            a-env-att [0.0 0.0 2.0]
            a-env-dec [0.0 0.02 0.2]
            a-env-sus [0.0 0.6 2.0]
            a-env-rel [0.0 0.1 1.0]

            ;; LFO for PWM
            lfo-rate [0.0 6.0 12.0]
            lfo-depth [0.0 0.05 0.1]]
           (let [lfo (o/lf-tri lfo-rate)
                 ;; Oscillators
                 f1 (* freq (com/ugen-semitone-ratio (* -1.0 detune)))
                 f2 (* freq (com/ugen-semitone-ratio detune))
                 pulse1 (o/pulse f1 (+ pw1 (* lfo lfo-depth)))
                 pulse2 (o/pulse f2 (+ pw2 (* lfo lfo-depth)))
                 saw1 (o/saw f1)
                 saw2 (o/saw f2)
                 mixed-osc (o/mix [(* 0.6 saw1) (* 0.6 saw2) (* 0.4 pulse1) (* 0.4 pulse2)])

                 ;; Filter envelope
                 f-env (o/env-gen (o/adsr f-env-att f-env-dec f-env-sus f-env-rel) gate)
                 mod-cutoff (+ cutoff (* env-amt f-env))

                 ;; Filtered tone
                 svf (o/rlpf mixed-osc mod-cutoff res)
                 filtered (o/x-fade2 mixed-osc svf svf-mix)

                 ;; Amp envelope
                 amp-env (o/env-gen (o/adsr a-env-att a-env-dec a-env-sus a-env-rel) gate :action o/FREE)
                 final (* filtered amp-env amp)

                 ;; Output
                 out (o/pan2 final pan)]
             (o/out 0 out))
           {:mono? true}))

