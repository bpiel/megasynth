(ns megasynth.synths.multiple
  (:require [megasynth.common :as com]
            [overtone.live :as o]
            ))




(swap! com/synths-ifaces&
       assoc :multiple
       (com/mk-synth-iface
         [freq [30.0 100.0 440.0]
          amp [0.01 1.0 0.4]
          even [0.0 1.0 0.5]
          odd [0.0 1.0 0.5]
          noise-amt [0.0 1.0 0.3]
          decay [0.05 2.0 0.6]
          filter-brightness [200.0 8000.0 2000.0]]
         
         (let [;;gate 1

               ;; Harmonic shaping envelopes
               env (o/env-gen (o/perc 0.005 decay) gate :action o/FREE)
               decay-curve (map (fn [mult]
                                  (* (o/env-gen (o/perc 0.005 (* decay (/ 1.0 mult))) gate)
                                     (o/sin-osc (* freq mult))))
                                [1 2 3 4 5 6 7 8])
               
               ;; Separate even and odd partials
               evens (->> decay-curve (filter #(even? (+ 1 (.indexOf decay-curve %)))) (apply +))
               odds  (->> decay-curve (filter #(odd?  (+ 1 (.indexOf decay-curve %)))) (apply +))
               
               harmonic-mix (o/mix [(o/mul-add even evens 0.0)
                                    (o/mul-add odd odds 0.0)])
               
               ;; Transient noise burst
               noise-env (o/env-gen (o/perc 0.001 0.02) gate)
               noise (* noise-amt noise-env (o/white-noise))
               
               ;; Resonant filter sweep to simulate body resonance
               cutoff (+ 100 (o/mul-add filter-brightness env 0.0))
               filtered (o/rlpf (+ harmonic-mix noise) cutoff 0.8)
               
               sig (* amp filtered env)]
           
           (o/out 0 sig))

         {:mono? false}))

#_
(swap! com/synths-ifaces&
       assoc :multiple
       (com/mk-synth-iface
           [amp [0.01 1.0 2.0]
            even [0.0 1.0 5.0]
            odd [0.0 1.0 5.0]
            noise-amt [0.0 1.0 2.0]
            decay [0.05 2.0 4.0]
            filter-brightness [200.0 8000.0 16000.0]]
         
           (let [;; Harmonic shaping envelopes
                 env (o/env-gen (o/perc 0.005 decay) gate :action o/FREE)
                 decay-curve-even (mapv (fn [mult]
                                          (* (o/env-gen (o/perc 0.005 (* decay (/ 1.0 mult))) gate)
                                             (o/sin-osc (* freq mult))))
                                        [2 4 6 8])
                 decay-curve-odd (mapv (fn [mult]
                                         (* (o/env-gen (o/perc 0.005 (* decay (/ 1.0 mult))) gate)
                                            (o/sin-osc (* freq mult))))
                                       [1 3 5 7])
               
                 ;; Separate even and odd partials
                 ;; evens (->> decay-curve (filter #(even? (+ 1 (.indexOf decay-curve %)))) (apply +))
                 ;; odds  (->> decay-curve (filter #(odd?  (+ 1 (.indexOf decay-curve %)))) (apply +))

                 even-mix (o/mix decay-curve-even)
                 odd-mix (o/mix decay-curve-odd)                 
                 harmonic-mix (o/mix [(o/mul-add even-mix even 0.0)
                                      (o/mul-add odd-mix odd 0.0)])
               
                 ;; Transient noise burst
                 noise-env (o/env-gen (o/perc 0.001 0.02) gate)
                 noise (* noise-amt noise-env (o/white-noise))
               
                 ;; Resonant filter sweep to simulate body resonance
                 cutoff (+ 100 (o/mul-add filter-brightness env 0.0))
                 filtered (o/rlpf (+ harmonic-mix noise) cutoff 0.8)
               
                 sig (* amp filtered env)]
           
             (o/out 0 sig))

           {:mono? false}))

#_
(swap! com/synths-ifaces&
       assoc :rich-timbre
       (com/mk-synth-iface
           [amp [0.01 1.0 0.5]
            decay [0.1 4.0 1.5]
            even [0.0 1.0 0.8]
            odd [0.0 1.0 0.5]
            even-falloff [0.1 3.0 1.0]
            odd-falloff [0.1 3.0 1.5]
            brilliance [0.0 1.0 0.7]
            detune-amt [0.0 1.0 0.1]
            spread [0.0 1.0 0.5]
            transient-noise-amt [0.0 1.0 0.2]
            filter-brightness [200.0 10000.0 3000.0]]

           (let [gate 1
                 env (o/env-gen (o/perc 0.005 decay) gate :action o/FREE)
                 base-freq freq
                 rand-detune #(o/mul-add (o/lf-noise1:kr 0.2) detune-amt 1.0)
                 partials (range 1 21)

                 ;; Generate harmonics with shaped envelopes and detune
                 gen-partials
                 (fn [partial-type falloff gain]
                   (->> partials
                        (filter #(= (mod % 2) partial-type)) ;; 0 = even, 1 = odd
                        (mapv
                         (fn [n]
                           (let [n* (float n)
                                 detuned (o/* base-freq n* (rand-detune))
                                 amp-scale (o/div 1.0 (o/pow n* falloff))
                                 env-scale (o/env-gen (o/perc 0.005 (o/div decay n*)) gate)
                                 pan-pos (o/- (o/* spread (/ n* 20.0)) 0.5)
                                 sig (o/* gain amp-scale env-scale (o/sin-osc detuned))]
                             (o/pan2 sig pan-pos))))))

                 even-partials (gen-partials 0 even-falloff even)
                 odd-partials  (gen-partials 1 odd-falloff odd)
                 all-partials (o/mix (concat even-partials odd-partials))

                 ;; Brilliance modulates high-passed signal
                 bright-env (o/env-gen (o/perc 0.005 (o/* decay 0.5)) gate)
                 bright-harmonics (o/rlpf all-partials
                                          (o/+ 5000 (o/* 5000 brilliance bright-env))
                                          0.8)

                 ;; Noise burst
                 noise-env (o/env-gen (o/perc 0.001 0.05) gate)
                 noise (o/* transient-noise-amt noise-env (o/white-noise))

                 ;; Final mix and filter shaping
                 cutoff (o/+ 200 (o/* filter-brightness env))
                 body-filtered (o/rlpf (o/+ all-partials bright-harmonics noise)
                                       cutoff 0.9)

                 final-sig (o/* amp body-filtered env)]

             (o/out 0 final-sig))

           {:mono? false}))



#_(o/stop)
