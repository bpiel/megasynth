(ns megasynth.chatgpt-v1
  (:use [overtone.live])
  (:require [clojure.set :as set]))

;; --- Global knob state + control function registry ---

(defonce knobs (atom {}))         ;; {:knob-name value}
(defonce control-fns (atom {}))   ;; {:knob-name fn-to-call-on-update}

(defn set-knob! [k v]
  (swap! knobs assoc k v)
  (when-let [ctl-fn (get @control-fns k)]
    (ctl-fn v)))

(defn register-ctl! [knob synth-id param-name]
  (swap! control-fns assoc knob #(ctl synth-id param-name %)))

;; --- Extended synth voice with LFO + modulated filter cutoff + envelope ---

(defsynth lfo-voice
  [freq 440 amp 0.5
   cutoff 1000 res 0.3
   lfo-rate 2.0 lfo-depth 300
   atk 0.01 dec 0.2 sus 0.7 rel 0.5
   gate 1 out-bus 0]

  (let [lfo (sin-osc:kr lfo-rate)
        mod-cutoff (+ cutoff (* lfo lfo-depth))
        env (env-gen (adsr atk dec sus rel) :gate gate :action FREE)
        osc (mix [(saw freq) (pulse (* freq 1.01))])
        filt (rlpf osc mod-cutoff res)
        sig (* amp env filt)]
    (out out-bus sig)))

;; --- Live voice control: one synth per ID, controlled via knobs ---

(defonce live-voices (atom {}))

(defn register-all-controls! [id synth-id]
  (doseq [[k param] { :freq :freq
                      :amp :amp
                      :cutoff :cutoff
                      :res :res
                      :lfo-rate :lfo-rate
                      :lfo-depth :lfo-depth
                      :atk :atk
                      :dec :dec
                      :sus :sus
                      :rel :rel }]
    (let [knob-name (keyword (str id "-" (name k)))]
      (register-ctl! knob-name synth-id param))))

(defn play-voice!
  [{:keys [id freq amp cutoff res
           lfo-rate lfo-depth
           atk dec sus rel]}]
  (let [s (lfo-voice :freq freq :amp amp :cutoff cutoff :res res
                     :lfo-rate lfo-rate :lfo-depth lfo-depth
                     :atk atk :dec dec :sus sus :rel rel)]
    (swap! live-voices assoc id s)
    (register-all-controls! id s)
    s))

;; --- Example usage ---

(comment
  
  ;; Start a synth with LFO-modulated filter cutoff
  (play-voice!
    {:id :voice1
     :freq 440 :amp 0.4 :cutoff 800 :res 0.2
     :lfo-rate 4.0 :lfo-depth 200
     :atk 0.01 :dec 0.1 :sus 0.8 :rel 0.3})

  ;; Live-modulate parameters
  (set-knob! :voice1-cutoff 300)
  (set-knob! :voice1-lfo-rate 8.0)
  (set-knob! :voice1-lfo-depth 500)
  (set-knob! :voice1-amp 0.2)
  (set-knob! :voice1-atk 0.05)
  (set-knob! :voice1-sus 0.9)

  (stop)
)
