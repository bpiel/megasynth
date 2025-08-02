(ns megasynth.synths.garson
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(defn vibrato [sig freq rate depth]
  (let [mod (o/mul-add (o/sin-osc:kr rate)
                       depth
                       1)
        freq*mod (o/mul-add mod freq 0.0)
        sin (o/sin-osc freq*mod)
        r (o/mul-add sig sin 0.0)]
    r))


#_(swap! com/synths-ifaces&
       assoc :garson
       (com/mk-synth-iface
           [amp [0.01 1.0 3.0]
            drive [0.01 2.0 6.0]
            cutoff [100 1200 2400]
            pan [-1.0 0.0 1.0]]
           (let [raw (* amp (+ (o/saw freq) (o/saw (* 0.99 freq))))
                 env (o/env-gen (o/adsr 0.5 0.5 0.6 1.5) :gate gate :action o/FREE)
                 sig (* raw env)
                 saturated (o/tanh (* drive sig))
                 filtered (o/rlpf saturated cutoff 0.3)
                 wowed (vibrato filtered freq 0.3 0.01)
                 final (* 0.8 wowed)]
             (o/out 0 (o/pan2 final pan)))
           {:mono? true}))

(swap! com/synths-ifaces&
       assoc :garson
       (com/mk-synth-iface
           [cutoff [100 1200 2400]
            cutoff-spread [1 5 10]            
            lfo-freq [0.01 0.3 1.0]
            lfo-amp [0.001 0.01 0.1]
            flutter-freq [0.1 5.2 10.0]
            flutter-amp [0.0001 0.005 0.01]
            cn-max-delay-t [0.01 0.4 0.8]
            cn-delay-t [0.01 0.4 0.8]
            cn-decay-t [0.01 4.0 8.0]                        
            drive [0.01 2.0 6.0]            
            pan [-1.0 0.0 1.0]
            amp [0.01 1.0 3.0]]
           (let [lfo (o/mul-add (o/sin-osc:kr lfo-freq) lfo-amp 1.0)
                 flutter (o/mul-add (o/sin-osc:kr flutter-freq) flutter-amp 1.0)
                 mod-freq (* freq lfo flutter)
                 raw (* amp (+ (o/saw freq) (o/saw (* 0.99 mod-freq))))
                 env (o/env-gen (o/adsr 0.5 0.5 0.6 1.5) :gate gate :action o/FREE)
                 sig (* raw env)
                 saturated (o/tanh (* drive sig))
                 filtered0 (o/rlpf saturated (/ cutoff cutoff-spread) 0.3)
                 filtered1 (o/rlpf saturated cutoff 0.3)
                 filtered2 (o/rlpf saturated (* cutoff-spread cutoff) 0.3)
                 filtered (o/mix [filtered0
                                  filtered1
                                  filtered2])
                 wowed (vibrato filtered mod-freq 0.3 0.01)
                 delayed (o/comb-n wowed
                                   cn-max-delay-t
                                   cn-delay-t
                                   cn-decay-t)
                 final (o/distort (* 0.8 (+ wowed delayed)))]
             (o/out 0 (o/pan2 final pan)))
           {:mono? true}))

