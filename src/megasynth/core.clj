(ns megasynth.core
  (:require [overtone.live :as o])
  (:use [overtone.live])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment

  (o/definst mellow-lead [freq 440 dur 1]
    (let [env (o/env-gen (o/adsr 0.5 0.1 0.8 1) :gate 1 :action o/FREE)
          sig (* (o/saw freq) env)]
      (o/rlpf sig 1000 0.4)))           ; lowpass filter

  (o/demo (mellow-lead))

  (o/stop)


  #_  (defsynth analog-synth
        [freq 440
         cutoff 1000
         res 0.4
         detune 3
         amp 0.5
         gate 1]
        (let [env (env-gen (adsr 0.01 0.2 0.6 0.5) :gate gate :action FREE)
              osc1 (saw freq)
              osc2 (saw (+ freq detune))
              mixed (* 0.5 (+ osc1 osc2))
              filtered (rlpf mixed cutoff res)]
          (* amp env filtered)))


  (defsynth analog-synth
    [freq 440
     cutoff 1000
     res 0.4
     detune 3
     amp 0.5
     gate 1]
    (let [env (env-gen (adsr 0.01 0.2 0.6 0.5) :gate gate :action FREE)
          osc1 (saw freq)
          osc2 (saw (+ freq detune))
          mixed (* 0.5 (+ osc1 osc2))
          filtered (rlpf mixed cutoff res)
          signal (* amp env filtered)]
      (out 0 [signal signal])))

  (defsynth analog-synth2
    [freq 440
     base-cutoff 1000
     lfo-rate 1.0
     lfo-depth 500
     res 0.4
     detune 3
     amp 0.5
     gate 1]
    (let [env (env-gen (adsr 0.01 0.2 0.6 0.5) :gate gate :action FREE)
          osc1 (saw freq)
          osc2 (saw (+ freq detune))
          mixed (* 0.5 (+ osc1 osc2))
          lfo (sin-osc:kr lfo-rate)
          cutoff (+ base-cutoff (* lfo-depth lfo)) ;; LFO modulates cutoff
          filtered (rlpf mixed cutoff res)
          signal (* amp env filtered)]
      (out 0 [signal signal])))
  

(defsynth analog-synth3
  [freq 440
   base-cutoff 1000
   cutoff-lfo-rate 0.5
   cutoff-lfo-depth 800
   vibrato-rate 6.0
   vibrato-depth 5
   res 0.4
   detune 3
   amp 0.5
   gate 1]
  (let [env (env-gen (adsr 0.01 0.2 0.6 0.5) :gate gate :action FREE)

        ;; Pitch LFO (vibrato)
        vibrato (sin-osc:kr vibrato-rate)
        mod-freq (+ freq (* vibrato-depth vibrato))

        ;; Detuned saws
        osc1 (saw mod-freq)
        osc2 (saw (+ mod-freq detune))
        mixed (* 0.5 (+ osc1 osc2))

        ;; Cutoff LFO (wah effect)
        cutoff-lfo (sin-osc:kr cutoff-lfo-rate)
        cutoff (+ base-cutoff (* cutoff-lfo-depth cutoff-lfo))

        ;; Filtered signal
        filtered (rlpf mixed cutoff res)
        signal (* amp env filtered)]

    (out 0 [signal signal])))


  
  (def my-voice (analog-synth :freq 220))

  (def my-voice2 (analog-synth :freq 400))

  (def my-voice3 (analog-synth2 :freq 220))

  (def my-voice4 (analog-synth3 :freq 220))  

  (ctl my-voice4 :vibrato-rate 1)
  (ctl my-voice4 :vibrato-depth 5)  
  (ctl my-voice4 :cutoff-lfo-rate 1.85)
  (ctl my-voice4 :cutoff-lfo-depth 800)

  ;; Increase cutoff (make it brighter)

  ;; Use `ctl` to change its parameters in real-time:
  (ctl my-voice :cutoff 2000)
  (ctl my-voice :res 0.8)
  (ctl my-voice :detune 5)
  (ctl my-voice :freq 330)
  (ctl my-voice :gate 0) ;; if you want to stop it
  (comment))
