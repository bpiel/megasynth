(ns megasynth.scratch1
  (:require [overtone.live :as o])
  (:use [overtone.live]))

;; --------------------------------------------------
;; 🚀 STARTUP: Boot SuperCollider audio engine
;; --------------------------------------------------

#_(boot-external-server) ; or (boot-internal-server)

;; --------------------------------------------------
;; 🎛️ VIRTUAL KNOBS (stateful parameter control)
;; --------------------------------------------------

(defonce knobs (atom {:freq 440 :cutoff 1000 :lfo-rate 2 :lfo-depth 300}))

(defn update-knob! [k v]
  (swap! knobs assoc k v))

;; --------------------------------------------------
;; 🎚️ PATCHABLE SYNTH (modulated filter example)
;; --------------------------------------------------

(defsynth mod-synth [freq 440 cutoff 1000 lfo-rate 2 lfo-depth 300 amp 0.4]
  (let [lfo      (sin-osc lfo-rate)
        mod-cut  (+ cutoff (* lfo lfo-depth))
        src      (saw freq)
        filt     (rlpf src (max 80 mod-cut) 0.3)
        env      (env-gen (adsr 0.01 0.2 0.8 1.0) :gate 1 :action FREE)]
    (out 0 (* env amp filt))))

;; --------------------------------------------------
;; 🎵 PLAYBACK CONTROL WITH HANDLE
;; --------------------------------------------------

(defonce current-synth (atom nil))

(defn play-patch []
  (when-let [old @current-synth]
    (ctl old :gate 0))
  (let [{:keys [freq cutoff lfo-rate lfo-depth]} @knobs
        new (mod-synth :freq freq :cutoff cutoff :lfo-rate lfo-rate :lfo-depth lfo-depth)]
    (reset! current-synth new)))

;; --------------------------------------------------
;; 🧪 TEST EXAMPLE
;; --------------------------------------------------
;; Call this to play a note with current knob settings:
;;
(play-patch)

;; Change parameters and hear live differences by retriggering:
;;
(update-knob! :cutoff 2000)

;;
(play-patch)

;; Or dynamically control a running synth:
;;
(ctl @current-synth :cutoff 1500)

(stop)
