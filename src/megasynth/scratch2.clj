(ns megasynth.scratch2
  (:require [megasynth.common :as com]
            [overtone.live :as o]))

(defn play [synth dur-ms & args]
  (let [vc (apply synth args)]
    (future
      (Thread/sleep dur-ms)
      (o/ctl vc :gate 0.0))
    true))

(o/defsynth around-the-world-lead-2
  [freq 110
   gate 1

   ;; Amp
   amp 0.8
   pan 0.0

   ;; Detune and pulse width
   detune 0.02
   pw1 0.39        ;; ~9:30 clock (range 0.0 to 1.0)
   pw2 0.5         ;; ~12:05 clock

   ;; Filter
   cutoff 400
   res 0.5
   env-amt 800
   svf-mix 0.7     ;; blend between osc and filtered

   ;; Filter envelope
   f-env-att 0.0
   f-env-dec 0.02
   f-env-sus 0.0
   f-env-rel 0.0

   ;; Amp envelope
   a-env-att 0.0
   a-env-dec 0.02
   a-env-sus 0.6
   a-env-rel 0.1

   ;; LFO for PWM
   lfo-rate 6.0
   lfo-depth 0.05]
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
    (o/out 0 out)))


(o/defsynth volca-keys0
  [freq 440
   gate 1

   amp 0.9
   detune 0.0

   ;; Filter
   cutoff 1200
   res 0.7
   env-amt 1000

   ;; Envelopes
   attack 0.01
   decay 0.2
   sustain 0.5
   release 0.3
   
   ;; Delay
   delay-time 0.25
   delay-fb 0.4
   delay-mix 0.3

   ;; LFO
   lfo-rate 5.0              ;; Hz
   lfo-depth 1.0             ;; global depth scaler
   lfo->pitch 0.0            ;; amount in semitones
   lfo->cutoff 0.0           ;; amount added to cutoff
   lfo->amp 0.0              ;; amplitude modulation depth

   pan 0.0]
  (let [;; LFO signal (triangle wave)
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
    (o/out 0 out)))

#_
(play volca-keys0 1000
      :sustain 1.0
      :decay 3.0
      :detune 0.5
      :lfo-rate 5.0
      :lfo->pitch 0.0
      :lfo->cutoff 1.0
      :lfo->amp 0.0
      :cutoff 4000
      :delay-time 0.27
      :delay-fb 1.0
      :delay-mix 0.0)

(o/defsynth around-the-world-lead
    [freq 110
     amp 1.2
     detune 0.04
     gate 1
     cutoff 1000
     res 0.6
     env-amt 2000
     attack 0.005
     decay 0.2
     sustain 0.0
     release 0.1
     lfo-rate 3.0
     lfo-depth 0.0
     dist-amt 3.0
     pan 0.0]
    (let [ ;; Pitch mod LFO (optional)
          lfo (o/lf-tri lfo-rate)
          pitch-lfo (* freq lfo lfo-depth)
          base-freq (+ freq pitch-lfo)

          ;; Detuned saws
          f1 (* base-freq (com/ugen-semitone-ratio (- detune)))
          f2 (* base-freq (com/ugen-semitone-ratio detune))
          saws [(o/pan2 (o/saw f1) -0.3)
                (o/pan2 (o/saw f2)  0.3)]
          mixed (o/mix saws)

          ;; Filter envelope
          filt-env (o/env-gen (o/adsr attack decay sustain release) gate)
          cutoff-mod (+ cutoff (* env-amt filt-env))
          filtered (o/bpf mixed cutoff-mod res)

          ;; Distortion
          driven (o/tanh (* filtered dist-amt))

          ;; Amp envelope
          amp-env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
          final (* driven amp-env amp)]

      (o/out 0 (o/pan2 final pan))))

(def default-synth1
  (o/synth [freq 440
            amp 0.7
            attack 0.1
            decay 0.9
            sustain 1.0
            release 0.9
            gate 1.0]
    (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
          sig (* amp env (o/square freq))]
      (o/out 0 (o/pan2 sig)))))


(def default-synth2
  (o/synth [freq 440
            amp 0.5
            attack 0.1
            decay 0.9
            sustain 1.0
            release 0.9
            gate 1.0]
    (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
          x (o/mix [(o/sin-osc freq)
                    (o/sin-osc (* 1.5 freq))
                    (o/sin-osc (* 2 freq))])
          sig (* 2.0 amp env x)]
      (o/out 0 (o/pan2 sig)))))

(def default-synth3
  (o/synth [freq 440
            amp 0.3
            attack 0.1
            decay 0.0
            sustain 1.0
            release 0.0
            gate 1.0]
    (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/NO-ACTION)
          env2 (o/env-gen (o/adsr attack 1.0 sustain 1.0) gate :action o/FREE)
          sig1 (* amp env (o/sin-osc freq))
          sig2 (* amp env2 (o/sin-osc (* 1.5 freq)))
          x (+ sig1 sig2)]
      (o/out 0 x))))

#_(default-synth2)

#_(o/stop)

#_(play default-synth2 400)


(o/defsynth da-funk0
  [freq 440 dur 1.0 amp 1.0 cutoff 2200 boost 12 dist-level 0.015]
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
    (o/out 0 sig)))

(o/defsynth da-funk-cgpt0
  [freq 440 dur 1.0 amp 1.0 cutoff 2200 boost 12 dist-level 0.015]
  (let [env (o/env-gen (o/adsr 0.01 0.1 0.8 0.05) (o/line:kr 1.0 0.0 dur) :action o/FREE)
        level (+ (* freq 0.25)
                 (o/env-gen (o/adsr 0.2 0.1 1 0.2) (o/line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff))
        osc (o/mix [(o/saw freq)
                    (o/saw (* freq 0.7491535384383409))])
        sig (-> osc
                (o/bpf level 0.6)
                (* env amp)
                o/pan2
                (o/clip2 dist-level)
                (* boost)
                o/distort)]
    (o/out 0 sig)))


(comment
  (let [sn0 (o/synth [freq 440
                      amp 0.5
                      attack 0.9
                      decay 2.0
                      sustain 0.0
                      release 2.0
                      gate 1.0]
                     (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
                           sig (* amp env (o/sin-osc freq))]
                       (o/out 0 (o/pan2 sig))))
        rng (apply concat (repeat 4 (range 1 10)))
        fs (map (fn [i] (* i 110)) rng)
        fs' (shuffle fs)]
    (reduce (fn [agg item]
              (play sn0 600 :freq item)
              (Thread/sleep 200)
              nil)
            nil
            fs'))
  (comment))

(o/defsynth fc-lead-342
  [freq 440
   amp 0.6
   detune 0.07
   pan-spread 0.4
   pitch-env-amt 0.0 ;; Set to >0 to activate pitch sweep
   amp-attack 0.01
   amp-decay 0.1
   amp-sustain 0.8
   amp-release 0.2
   rev-mix 0.3
   rev-room 0.5
   gate 1]
  (let [; Detuned saws
        f1 (* freq (com/ugen-semitone-ratio (* -1.0 detune)))
        f2 (* freq (com/ugen-semitone-ratio -0.03))
        f3 (* freq (com/ugen-semitone-ratio 0.03))
        f4 (* freq (com/ugen-semitone-ratio detune))
        saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
              (o/pan2 (o/saw f2) (* -0.3 pan-spread))
              (o/pan2 (o/saw f3) (* 0.3 pan-spread))
              (o/pan2 (o/saw f4) pan-spread)]
        mixed (o/mix saws)

        ; Pitch envelope (optional)
        pitch-env (o/env-gen (o/adsr 0.0 0.01 0.0 0.01) gate)
        pitch-mod (com/ugen-semitone-ratio (* pitch-env pitch-env-amt))
        pitched (* mixed pitch-mod)

        ; Amp envelope
        amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                           gate :action o/FREE)
        voiced (* pitched amp-env amp)

        ; Reverb
        wet (o/free-verb voiced rev-mix rev-room)]
    (o/out 0 wet)))



#_
(play fc-lead-342 400 :amp 2.0 :pitch-env-amt 18.0)

#_
(o/stop)

#_
(play final-countdown-claude0 440)

#_
(play final-countdown2 440)

#_
(play final-countdown2 440)




(o/defsynth kick-drum [amp 0.5]
  (let [env (o/env-gen (o/perc 0.01 0.3) :action o/FREE)
        freq-env (o/env-gen (o/envelope [60 30] [0.1]))
        osc (o/sin-osc freq-env)]
    (o/out 0 (* osc env amp))))

(o/defsynth final-countdown-claude0 [freq 440 amp 0.4 gate 1 cutoff 3000 res 0.3]
  (let [env (o/env-gen (o/adsr 0.02 0.3 0.7 0.5) gate :action o/FREE)
        ; Layer multiple oscillators for richness
        osc1 (o/saw freq)
        osc2 (o/saw (* freq 1.01)) ; Slight detune for width
        osc3 (o/pulse freq 0.3)    ; Add some pulse wave character
        
        ; mix the oscillators
        mixed (+ (* osc1 0.4) (* osc2 0.3) (* osc3 0.3))
        
        ; Filter with envelope modulation
        filter-env (o/env-gen (o/adsr 0.01 0.4 0.6 0.3) gate)
        filtered (o/rlpf mixed (+ cutoff (* filter-env 2000)) res)
        
        ; Add some bite with distortion
        distorted (o/tanh (* filtered 2))
        
        ; Final output with some stereo width
        left (* distorted env amp)
        right (* distorted env amp 0.9)]
    (o/out 0 [left right])))

#_
(play final-countdown-claude0 400)

#_
(o/defsynth final-countdown-lead [freq 440 amp 0.4 gate 1]
  (let [; Amp envelope - quick attack, full sustain, short release
        amp-env (o/env-gen #_(o/envelope [0 1 1 0] [0.01 0.5 0.08] :sustain 1)
                           (o/envelope [0 1 1 0] [0.01 0.5 0.08])
                           gate :action o/FREE)
        
        ; Pitch modulation envelope - 18 semitone drop over 10ms
        pitch-env (o/env-gen (o/envelope [18 0] [0.01])
                             :action o/NO-ACTION)
        modulated-freq (* freq (o/midi->hz pitch-env))
        
        ; 4-voice unison saw waves with 7-cent detune spread
        detune-cents 7
        detune-ratio (Math/pow 2 (/ detune-cents 1200))
        
        osc1 (o/saw modulated-freq)
        osc2 (o/saw (* modulated-freq detune-ratio))
        osc3 (o/saw (* modulated-freq (/ 1 detune-ratio)))
        osc4 (o/saw (* modulated-freq (* detune-ratio detune-ratio)))
        
        ; Mix unison voices with stereo spread
        mixed (+ osc1 osc2 osc3 osc4)
        
        ; Apply amp envelope
        shaped (* mixed amp-env amp)
        
        ; Add reverb (30% wet, medium size)
        reverbed (+ (* shaped 0.7) (* (o/free-verb shaped 0.5 0.5 0.5) 0.3))
        
        ; Stereo output with slight spread
        left reverbed
        right (* reverbed 0.9)]
    
    (o/out 0 [left right])))

(o/defsynth fc-lead-chatgpt0
  [freq 440
   amp 0.5
   gate 1
   cutoff 2500
   res 0.25
   detune 0.01
   pulse-width 0.15
   drive 2.0
   pan 0.0]
  (let [; Envelopes
        amp-env (o/env-gen (o/adsr 0.005 0.1 0.85 0.1) gate :action o/FREE)
        filter-env (o/env-gen (o/adsr 0.005 0.2 0.6 0.1) gate)
        
        ; Oscillators
        saw1 (o/saw freq)
        saw2 (o/saw (* freq (+ 1 detune)))
        saw3 (o/pulse freq pulse-width)
        mixed (o/mix [(* saw1 0.5)
                      (* saw2 0.3)
                      (* saw3 0.3)])
        
        ; Filter with envelope modulation
        filt (o/rlpf mixed (+ cutoff (* 2000 filter-env)) res)
        
        ; Distortion and panning
        driven (o/tanh (* filt drive))
        sig (* driven amp-env amp)]
    
    (o/out 0 (o/pan2 sig pan))))

#_
(play fc-lead-chatgpt0 400 :amp 2.0)

(o/defsynth fc-lead-501
  [freq 440
   amp 0.6
   detune 0.07
   pan-spread 0.4
   pitch-env-amt 18.0 ;; Set to >0 to activate pitch sweep
   amp-attack 0.01
   amp-decay 0.1
   amp-sustain 0.8
   amp-release 0.2
   rev-mix 0.3
   rev-room 0.5
   gate 1]
  (let [ ;; Pitch envelope (optional)
        pitch-env (o/env-gen (o/adsr 0.0 0.06 0.0 0.01) gate)
        pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
        freq' (* freq pitch-mod)
        ;; Detuned saws
        f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
        f2 (* freq' (ugen-semitone-ratio -0.03))
        f3 (* freq' (ugen-semitone-ratio 0.03))
        f4 (* freq' (ugen-semitone-ratio detune))
        saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
              (o/pan2 (o/saw f2) (* -0.3 pan-spread))
              (o/pan2 (o/saw f3) (* 0.3 pan-spread))
              (o/pan2 (o/saw f4) pan-spread)]
        mixed (o/mix saws)
        
        ;; Amp envelope
        amp-env (o/env-gen (o/adsr 0.1 amp-decay amp-sustain amp-release)
                           gate :action o/FREE)
        voiced (* mixed amp-env amp)

        ;; Reverb
        wet (o/free-verb voiced rev-mix rev-room)]
    (o/out 0 wet)))

(def fc-lead-x
  (o/synth 
      [freq 440
       amp 2
       detune 0.07
       pan-spread 0.4
       pitch-env-amt 9.0 ;; Set to >0 to activate pitch sweep
       amp-attack 0.05
       amp-decay 0.1
       amp-sustain 0.8
       amp-release 0.2
       rev-mix 0.3
       rev-room 0.6
       gate 1]
      (let [ ;; Pitch envelope (optional)
            pitch-env (o/env-gen (o/adsr 0.0 0.06 0.0 0.01) gate)
            pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
            freq' (* freq pitch-mod)
            ;; Detuned saws
            f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
            f2 (* freq' (ugen-semitone-ratio -0.03))
            f3 (* freq' (ugen-semitone-ratio 0.03))
            f4 (* freq' (ugen-semitone-ratio detune))
            saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
                  (o/pan2 (o/saw f2) (* -0.3 pan-spread))
                  (o/pan2 (o/saw f3) (* 0.3 pan-spread))
                  (o/pan2 (o/saw f4) pan-spread)]
            mixed (o/mix saws)
                      
            ;; Amp envelope
            amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                               gate :action o/FREE)
            voiced (* mixed amp-env amp)

            ;; Reverb
            wet (o/free-verb voiced rev-mix rev-room)]
        (o/out 0 wet))))

(def fc-lead-x2
  (o/synth 
      [freq 440
       amp 2
       detune 0.07
       pan-spread 0.4
       pitch-env-amt 9.0 ;; Set to >0 to activate pitch sweep
       amp-attack 0.05
       amp-decay 0.1
       amp-sustain 0.8
       amp-release 0.2
       rev-mix 0.3
       rev-room 0.6
       gate 1]
      (let [pitch-env (o/env-gen (o/adsr 0.0 0.025 0.0 0.01) gate)
            pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
            freq' (* freq pitch-mod)

            ;; Detuned saws
            f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
            f2 (* freq' (ugen-semitone-ratio -0.03))
            f3 (* freq' (ugen-semitone-ratio 0.03))
            f4 (* freq' (ugen-semitone-ratio detune))
            saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
                  (o/pan2 (o/saw f2) (* -0.3 pan-spread))
                  (o/pan2 (o/saw f3) (* 0.3 pan-spread))
                  (o/pan2 (o/saw f4) pan-spread)]
            mixed (o/mix saws)

            ;; Filter envelope
            filter-env (o/env-gen (o/adsr 0.01 0.1 0.0 0.1) gate)
            filtered (o/rlpf mixed
                             (+ 2000 (* 2000 filter-env)) ;; opens from 2000 to ~4000
                             0.1)

            driven (o/tanh (* filtered 3)) ;; distortion
            amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                               gate :action o/FREE)
            voiced (* driven amp-env amp)
            wet (o/free-verb voiced rev-mix rev-room)]
        (o/out 0 wet))))

(def fc-lead-x3
  (o/synth 
    [freq 440
     amp 1.5
     detune 0.07
     pan-spread 0.4
     pitch-env-amt 12.0 ;;9.0
     amp-attack 0.05
     amp-decay 0.1
     amp-sustain 0.8
     amp-release 0.2
     cutoff-base 4000 ;; 2000
     cutoff-env-amt 4000 ;; 2000
     res 0.1 ;; 0.1
     rev-mix 0.3
     rev-room 0.6
     gate 1]
    (let [;; Pitch sweep
          pitch-env (o/env-gen (o/adsr 0.0 0.06 0.0 0.01) gate)
          pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
          freq' (* freq pitch-mod)

          ;; Detuned saws
          f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
          f2 (* freq' (ugen-semitone-ratio -0.03))
          f3 (* freq' (ugen-semitone-ratio 0.03))
          f4 (* freq' (ugen-semitone-ratio detune))
          saws [(o/pan2 (o/saw f1) (* -1.0 pan-spread))
                (o/pan2 (o/saw f2) (* -0.3 pan-spread))
                (o/pan2 (o/saw f3) (* 0.3 pan-spread))
                (o/pan2 (o/saw f4) pan-spread)]
          mixed (o/mix saws)

          ;; Filter for body fullness
          filter-env (o/env-gen (o/adsr 0.01 0.1 0.0 0.1) gate)
          cutoff (+ cutoff-base (* cutoff-env-amt filter-env))
          filtered (o/rlpf mixed cutoff res)

          ;; Light drive
          driven (o/tanh (* filtered 3))

          ;; Amp envelope
          amp-env (o/env-gen (o/adsr amp-attack amp-decay amp-sustain amp-release)
                             gate :action o/FREE)
          voiced (* driven amp-env amp)

          ;; Reverb
          wet (o/free-verb voiced rev-mix rev-room)]
      (o/out 0 wet))))


(def fc-lead-x4
  (o/synth 
      [freq 440
       amp 2
       detune 0.07
       pan-spread 0.4
       pitch-env-amt 6.0 ;; 9.0
       amp-attack 0.05
       amp-decay 0.1
       amp-sustain 0.8
       amp-release 0.2
       rev-mix 0.3
       rev-room 2.0 ;; 0.6
       pulse-width 0.3
       bpf-freq 1800 ;; 1400
       bpf-q 2.0     ;; 1.0
       dist-gain 4.0
       gate 1]
      (let [ ;; Pitch envelope
            pitch-env (o/env-gen (o/adsr 0.0 0.05 0.0 0.01) gate)
            pitch-mod (ugen-semitone-ratio (* pitch-env pitch-env-amt))
            freq' (* freq pitch-mod)

            ;; Oscillators: saw + pulse blend
            f1 (* freq' (ugen-semitone-ratio (* -1.0 detune)))
            f2 (* freq' (ugen-semitone-ratio detune))
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
        (o/out 0 wet))))

(comment)


(comment
  (let [pea 5.0]
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 200)
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 493)
    (Thread/sleep 200)
    (play fc-lead-501 500 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 500)
    (play fc-lead-501 1000 :amp 2.0 :pitch-env-amt pea :freq 369)
    (Thread/sleep 1500)
    
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 587)
    (Thread/sleep 200)
    (play fc-lead-501 200 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 200)
    (play fc-lead-501 400 :amp 2.0 :pitch-env-amt pea :freq 587)
    (Thread/sleep 400)
    (play fc-lead-501 400 :amp 2.0 :pitch-env-amt pea :freq 554)
    (Thread/sleep 400)
    (play fc-lead-501 1000 :amp 2.0 :pitch-env-amt pea :freq 493))
  
  (comment))

#_
(play fc-lead-chatgpt0 440)

(o/defsynth detune-test
  [freq 440 amp 0.7 detune 0.07 gate 1]
  (let [;;f1 (* freq (ugen-semitone-ratio (* -1.0 detune)))
        f2 freq
        f3 (* freq (ugen-semitone-ratio detune))
        saws [#_(o/pan2 (o/saw f1))
              (o/pan2 (o/saw f2))
              (o/pan2 (o/saw f3))]
        mixed (o/mix saws)
        env (o/env-gen (o/adsr 0.01 0.3 1 0.5) :gate gate :action o/FREE)]
    (o/out 0 (* env mixed))))

#_
(play detune-test 500 :detune 0.07)

#_
(play  (o/synth [] (o/out 0 (o/saw 440))) 500)

#_
(o/stop)

#_
(o/kill @*da-funk0-voice)

#_
(test-env-gen3)

#_
(o/demo (temp1 660))

#_
(o/stop)

