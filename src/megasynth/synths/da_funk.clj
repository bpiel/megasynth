(ns megasynth.synths.da-funk
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(swap! com/synths-ifaces&
        assoc :da-funk
        (com/mk-synth-iface
            [amp [0.01 1.0 3.0]
             dur [0.01 1.0 10.0]
             cutoff [1 2200 6000]
             boost [0 12 24]
             dist-level [0.001 0.015 0.2]]
            (let [env (o/env-gen (o/adsr 0.3 0.7 0.5 0.3) (o/line:kr 1.0 0.0 dur) :action o/FREE)
                  level (+ (* freq 0.25)
                           (o/env-gen (o/adsr 0.5 0.3 1 0.5) (o/line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff))
                  osc (o/mix [(o/saw freq)
                              (o/saw (* freq 0.7491535384383409))])
                  sig (-> osc
                          (o/bpf level 0.6)
                          (* env amp)
                          o/pan2
                          (o/clip2 dist-level)
                          (* boost)
                          o/distort)]
              (o/out 0 sig))
            {:mono? true}))

