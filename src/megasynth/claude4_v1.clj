(ns megasynth.claude4-v1
  "DIY Synthesizer Platform - Foundation namespace with modular synth architecture
   Provides real-time parameter control, multiple oscillators, filters, LFOs, and effects"
  (:require [overtone.core :as o]
            [overtone.inst.synth :as synth]))

;; =============================================================================
;; GLOBAL STATE & PARAMETER MANAGEMENT
;; =============================================================================

(defonce synth-instances (atom {}))
(defonce global-params (atom {}))

(defn register-synth!
  "Register a synth instance for live control"
  [synth-key synth-instance]
  (swap! synth-instances assoc synth-key synth-instance)
  synth-instance)

(defn get-synth
  "Get a registered synth instance by key"
  [synth-key]
  (get @synth-instances synth-key))

(defn live-ctl!
  "Control parameters of a registered synth in real-time"
  [synth-key & params]
  (when-let [synth-inst (get-synth synth-key)]
    (apply o/ctl synth-inst params)))

(defn stop-synth!
  "Stop and unregister a synth"
  [synth-key]
  (when-let [synth-inst (get-synth synth-key)]
    (o/kill synth-inst)
    (swap! synth-instances dissoc synth-key)))

(defn stop-all-synths!
  "Emergency stop - kill all registered synths"
  []
  (doseq [[k v] @synth-instances]
    (o/kill v))
  (reset! synth-instances {}))

;; =============================================================================
;; CORE SYNTHESIZER DEFINITION
;; =============================================================================

(o/defsynth modular-synth
  "Main modular synthesizer with multiple oscillators, filter, LFOs, and effects"
  [;; Oscillator 1
   osc1-freq    {:default 440 :min 20 :max 4000}
   osc1-detune  {:default 0 :min -100 :max 100}
   osc1-wave    {:default 0 :min 0 :max 3}      ; 0=saw, 1=square, 2=tri, 3=sine
   osc1-level   {:default 0.5 :min 0 :max 1}
   
   ;; Oscillator 2  
   osc2-freq    {:default 440 :min 20 :max 4000}
   osc2-detune  {:default 0 :min -100 :max 100}
   osc2-wave    {:default 0 :min 0 :max 3}
   osc2-level   {:default 0.3 :min 0 :max 1}
   osc2-sync    {:default 0 :min 0 :max 1}      ; Hard sync to osc1
   
   ;; Sub Oscillator
   sub-level    {:default 0.2 :min 0 :max 1}
   sub-octave   {:default 1 :min 1 :max 3}      ; Octaves below osc1
   
   ;; Noise
   noise-level  {:default 0 :min 0 :max 1}
   noise-type   {:default 0 :min 0 :max 2}      ; 0=white, 1=pink, 2=brown
   
   ;; Filter
   filter-cutoff  {:default 1000 :min 20 :max 8000}
   filter-res     {:default 0.3 :min 0 :max 1}
   filter-type    {:default 0 :min 0 :max 2}    ; 0=lpf, 1=hpf, 2=bpf
   filter-drive   {:default 1 :min 1 :max 4}
   
   ;; Filter Envelope  
   filter-env-amt {:default 0 :min -4000 :max 4000}
   filter-attack  {:default 0.01 :min 0.001 :max 4}
   filter-decay   {:default 0.3 :min 0.001 :max 4}
   filter-sustain {:default 0.5 :min 0 :max 1}
   filter-release {:default 1 :min 0.001 :max 8}
   
   ;; Amplitude Envelope
   amp-attack   {:default 0.01 :min 0.001 :max 4}
   amp-decay    {:default 0.3 :min 0.001 :max 4}
   amp-sustain  {:default 0.7 :min 0 :max 1}
   amp-release  {:default 1 :min 0.001 :max 8}
   
   ;; LFO 1 (Filter modulation)
   lfo1-rate    {:default 2 :min 0.01 :max 20}
   lfo1-depth   {:default 0 :min 0 :max 2000}
   lfo1-wave    {:default 0 :min 0 :max 3}      ; 0=sine, 1=tri, 2=saw, 3=square
   
   ;; LFO 2 (Pitch modulation) 
   lfo2-rate    {:default 4 :min 0.01 :max 20}
   lfo2-depth   {:default 0 :min 0 :max 100}
   lfo2-wave    {:default 0 :min 0 :max 3}
   
   ;; Effects
   chorus-rate    {:default 0.5 :min 0.1 :max 5}
   chorus-depth   {:default 0 :min 0 :max 1}
   chorus-mix     {:default 0 :min 0 :max 1}
   
   delay-time     {:default 0.25 :min 0.01 :max 2}
   delay-feedback {:default 0.3 :min 0 :max 0.9}
   delay-mix      {:default 0 :min 0 :max 1}
   
   reverb-room    {:default 0.5 :min 0 :max 1}
   reverb-damp    {:default 0.5 :min 0 :max 1}
   reverb-mix     {:default 0.2 :min 0 :max 1}
   
   ;; Global
   gate         {:default 1 :min 0 :max 1}
   master-vol   {:default 0.7 :min 0 :max 1}
   pan          {:default 0 :min -1 :max 1}]
   
  (let [        ;; LFO Generators
        lfo1 (o/select:ar lfo1-wave [(o/sin-osc lfo1-rate)
                                     (o/lf-tri lfo1-rate)  
                                     (o/lf-saw lfo1-rate)
                                     (o/lf-pulse lfo1-rate)])
               
        lfo2 (o/select:ar lfo2-wave [(o/sin-osc lfo2-rate)
                                     (o/lf-tri lfo2-rate)
                                     (o/lf-saw lfo2-rate) 
                                     (o/lf-pulse lfo2-rate)])
        
        ;; Pitch modulation
        osc1-pitch (+ osc1-freq osc1-detune (* lfo2 lfo2-depth))
        osc2-pitch (+ osc2-freq osc2-detune (* lfo2 lfo2-depth))
        
        ;; Oscillator 1
        osc1-sig (o/select:ar osc1-wave [(o/saw osc1-pitch)
                                         (o/pulse osc1-pitch)
                                         (o/lf-tri osc1-pitch)  
                                         (o/sin-osc osc1-pitch)])
        
        ;; Oscillator 2 (with optional hard sync)
        osc2-sig (let [base-osc (o/select:ar osc2-wave [(o/saw osc2-pitch)
                                                        (o/pulse osc2-pitch)
                                                        (o/lf-tri osc2-pitch)
                                                        (o/sin-osc osc2-pitch)])
                       sync-osc (o/sync-saw osc1-pitch osc2-pitch)]
                   (o/select:ar osc2-sync [base-osc sync-osc]))
        
        ;; Sub Oscillator
        sub-freq (/ osc1-freq sub-octave)
        sub-sig (* sub-level (o/pulse sub-freq))
        
        ;; Noise Generator
        noise-sig (* noise-level 
                     (o/select:ar noise-type [(o/white-noise)
                                             (o/pink-noise)  
                                             (o/brown-noise)]))
        
        ;; Mix Oscillators
        mixed-osc (+ (* osc1-level osc1-sig)
                     (* osc2-level osc2-sig)
                     sub-sig
                     noise-sig)
        
        ;; Envelopes
        filter-env (o/env-gen (o/adsr filter-attack filter-decay 
                                     filter-sustain filter-release)
                              gate)
        amp-env (o/env-gen (o/adsr amp-attack amp-decay 
                                  amp-sustain amp-release)
                           gate :action o/FREE)
        
        ;; Filter with modulation
        filter-freq (+ filter-cutoff 
                       (* filter-env-amt filter-env)
                       (* lfo1 lfo1-depth))
        
        filtered (o/select:ar filter-type [(o/moog-ff (* mixed-osc filter-drive) filter-freq filter-res)
                                           (o/hpf mixed-osc filter-freq)
                                           (o/bpf mixed-osc filter-freq filter-res)])
        
        ;; Effects Chain
        chorused (+ filtered 
                    (* chorus-mix 
                       (o/mix (for [i (range 4)]
                                (o/delay-c filtered 0.02 
                                          (+ 0.01 (* i 0.003) 
                                             (* chorus-depth 0.005 
                                                (o/sin-osc (+ chorus-rate (* i 0.1))))))))))
        
        delayed (+ chorused 
                   (* delay-mix 
                      (o/comb-c chorused delay-time delay-time delay-feedback)))
        
        reverbed (+ delayed 
                    (* reverb-mix 
                       (o/free-verb delayed reverb-room reverb-damp)))
        
        ;; Final output
        output (* reverbed amp-env master-vol)]
    
    (o/out 0 (o/pan2 output pan))))

;; =============================================================================
;; HIGH-LEVEL INTERFACE FUNCTIONS  
;; =============================================================================

(defn play-note!
  "Play a note with the modular synth and register it for live control"
  ([freq] (play-note! freq :main))
  ([freq synth-key]
   (let [synth-inst (modular-synth :osc1-freq freq :osc2-freq freq)]
     (register-synth! synth-key synth-inst))))

(defn play-chord!
  "Play multiple notes as a chord, each registered separately"
  [freqs & {:keys [base-key] :or {base-key :chord}}]
  (doall 
    (map-indexed 
      (fn [i freq]
        (let [synth-key (keyword (str (name base-key) "-" i))]
          (play-note! freq synth-key)))
      freqs)))

;; =============================================================================
;; PRESET MANAGEMENT
;; =============================================================================

(def default-lead-preset
  {:osc1-wave 0      ; Saw wave
   :osc2-wave 0      ; Saw wave  
   :osc2-detune 7    ; Slight detune
   :osc2-level 0.3
   :filter-cutoff 1200
   :filter-res 0.4
   :filter-env-amt 1500
   :filter-attack 0.01
   :filter-decay 0.4
   :filter-sustain 0.3
   :lfo1-rate 4
   :lfo1-depth 300
   :delay-time 0.375
   :delay-mix 0.2
   :reverb-mix 0.15})

(def default-bass-preset  
  {:osc1-wave 1      ; Square wave
   :sub-level 0.6    ; Prominent sub
   :filter-cutoff 400
   :filter-res 0.6
   :filter-drive 2
   :amp-attack 0.005
   :amp-decay 0.8
   :amp-sustain 0.3
   :amp-release 0.5})

(def default-pad-preset
  {:osc1-wave 0      ; Saw
   :osc2-wave 2      ; Triangle
   :osc2-detune -12  ; Octave down
   :osc1-level 0.4
   :osc2-level 0.4
   :filter-cutoff 800
   :filter-res 0.2
   :amp-attack 1.5
   :amp-decay 2
   :amp-sustain 0.8
   :amp-release 3
   :chorus-rate 0.3
   :chorus-depth 0.4
   :chorus-mix 0.6
   :reverb-mix 0.4})

(defn apply-preset!
  "Apply a preset to a registered synth"
  [synth-key preset]
  (apply live-ctl! synth-key (flatten (seq preset))))

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

(defn midi->freq
  "Convert MIDI note number to frequency"
  [midi-note]
  (* 440 (Math/pow 2 (/ (- midi-note 69) 12))))

(defn note->freq
  "Convert note name to frequency (e.g., :C4 -> 261.63)"
  [note]
  (get {:C0 16.35 :C#0 17.32 :D0 18.35 :D#0 19.45 :E0 20.60 :F0 21.83
        :F#0 23.12 :G0 24.50 :G#0 25.96 :A0 27.50 :A#0 29.14 :B0 30.87
        :C1 32.70 :C#1 34.65 :D1 36.71 :D#1 38.89 :E1 41.20 :F1 43.65
        :F#1 46.25 :G1 49.00 :G#1 51.91 :A1 55.00 :A#1 58.27 :B1 61.74
        :C2 65.41 :C#2 69.30 :D2 73.42 :D#2 77.78 :E2 82.41 :F2 87.31
        :F#2 92.50 :G2 98.00 :G#2 103.83 :A2 110.00 :A#2 116.54 :B2 123.47
        :C3 130.81 :C#3 138.59 :D3 146.83 :D#3 155.56 :E3 164.81 :F3 174.61
        :F#3 185.00 :G3 196.00 :G#3 207.65 :A3 220.00 :A#3 233.08 :B3 246.94
        :C4 261.63 :C#4 277.18 :D4 293.66 :D#4 311.13 :E4 329.63 :F4 349.23
        :F#4 369.99 :G4 392.00 :G#4 415.30 :A4 440.00 :A#4 466.16 :B4 493.88
        :C5 523.25 :C#5 554.37 :D5 587.33 :D#5 622.25 :E5 659.25 :F5 698.46
        :F#5 739.99 :G5 783.99 :G#5 830.61 :A5 880.00 :A#5 932.33 :B5 987.77
        :C6 1046.50} note 440))

;; =============================================================================
;; DEMO & TESTING FUNCTIONS
;; =============================================================================

(defn demo-basic!
  "Play a basic demo note"
  []
  (play-note! 440 :demo)
  (println "Demo note playing on 440Hz. Try: (live-ctl! :demo :filter-cutoff 2000)"))

(defn demo-chord!
  "Play a demo chord"
  []
  (play-chord! [261.63 329.63 392.00] :base-key :demo-chord)  ; C major
  (println "C major chord playing. Try: (live-ctl! :demo-chord-0 :lfo1-depth 500)"))

(defn demo-preset!  
  "Demo preset application"
  []
  (play-note! 220 :preset-demo)
  (Thread/sleep 500)
  (apply-preset! :preset-demo default-lead-preset)
  (println "Lead preset applied. Try different presets or modify parameters!"))

(comment
  ;; =============================================================================
  ;; USAGE EXAMPLES & REPL WORKFLOW
  ;; =============================================================================
  
  ;; Start Overtone (run once)
  ;; (o/boot-external-server)  ; or (o/boot-internal-server)
  
  ;; Basic playback
  (demo-basic!)
  (demo-chord!)
  (demo-preset!)

  (o/stop)
  
  ;; Manual note playing
  (play-note! 440 :test)                    ; Play A4
  (play-note! (note->freq :C4) :test2)      ; Play C4
  
  ;; Real-time parameter control
  (live-ctl! :test :filter-cutoff 2000)     ; Open filter
  (live-ctl! :test :lfo1-depth 800)         ; Add filter wobble
  (live-ctl! :test :osc2-detune 12)         ; Detune osc2
  (live-ctl! :test :delay-mix 0.4)          ; Add delay
  
  ;; Multiple parameter changes
  (live-ctl! :test 
             :filter-cutoff 1500
             :filter-res 0.7
             :lfo1-rate 6
             :reverb-mix 0.3)
  
  ;; Apply presets
  (apply-preset! :test default-lead-preset)
  (apply-preset! :test default-bass-preset)
  (apply-preset! :test default-pad-preset)
  
  ;; Chord playing with individual control
  (play-chord! [(note->freq :C3) (note->freq :E3) (note->freq :G3)] :base-key :chord)
  (live-ctl! :chord-0 :filter-cutoff 800)   ; Control just the root
  (live-ctl! :chord-1 :lfo2-depth 20)       ; Add vibrato to third
  
  ;; Stop synths
  (stop-synth! :test)
  (stop-all-synths!)  ; Emergency stop
  
  ;; Check what's running
  @synth-instances

  (def xs0 (-> @synth-instances vals first))

  #_(deref (.status xs0))
)
