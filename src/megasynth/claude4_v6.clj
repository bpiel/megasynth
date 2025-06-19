(ns megasynth.claude4-v6
  "Modular DIY Synthesizer with Device/Knob/Patch Matrix Architecture
   Provides composable devices, knob mapping, and flexible signal routing"
  (:require [overtone.core :as o]))

;; =============================================================================
;; GLOBAL STATE MANAGEMENT
;; =============================================================================

(defonce patch-matrix (atom {}))
(defonce device-registry (atom {}))
(defonce knob-registry (atom {}))
(defonce bus-registry (atom {}))
(defonce synth-node (atom nil))

;; =============================================================================
;; BUS MANAGEMENT SYSTEM
;; =============================================================================

(defn create-control-bus!
  "Create a control rate bus for CV signals"
  [bus-id]
  (let [bus (o/control-bus)]
    (swap! bus-registry assoc bus-id bus)
    (o/control-bus-set! bus 0)  ; Initialize to 0
    bus))

(defn create-audio-bus!
  "Create an audio rate bus for audio signals"
  [bus-id]
  (let [bus (o/audio-bus)]
    (swap! bus-registry assoc bus-id bus)
    bus))

(defn get-bus [bus-id]
  (get @bus-registry bus-id))

(defn set-bus-value! [bus-id value]
  (when-let [bus (get-bus bus-id)]
    (o/control-bus-set! bus value)))

;; =============================================================================
;; KNOB SYSTEM (0.0 - 1.0 normalized values)
;; =============================================================================

(defrecord Knob [id value min-val max-val curve])

(defn create-knob! 
  "Create a knob with 0.0-1.0 range that maps to actual parameter ranges"
  [knob-id & {:keys [min-val max-val curve] 
              :or {min-val 0 max-val 1 curve :linear}}]
  (let [knob (->Knob knob-id 0.5 min-val max-val curve)]
    (swap! knob-registry assoc knob-id knob)
    (create-control-bus! (keyword (str "knob-" (name knob-id))))
    knob))

(defn scale-knob-value
  "Convert 0.0-1.0 knob value to actual parameter range"
  [knob-value min-val max-val curve]
  (case curve
    :linear (+ min-val (* knob-value (- max-val min-val)))
    :exponential (+ min-val (* (Math/pow knob-value 2) (- max-val min-val)))
    :logarithmic (+ min-val (* (Math/sqrt knob-value) (- max-val min-val)))
    ;; Default linear
    (+ min-val (* knob-value (- max-val min-val)))))

(defn set-knob! 
  "Set knob value (0.0-1.0) and update connected parameters"
  [knob-id value]
  (when-let [knob (get @knob-registry knob-id)]
    (let [clamped-value (max 0.0 (min 1.0 value))
          updated-knob (assoc knob :value clamped-value)
          scaled-value (scale-knob-value clamped-value 
                                        (:min-val knob) 
                                        (:max-val knob) 
                                        (:curve knob))
          bus-id (keyword (str "knob-" (name knob-id)))]
      (swap! knob-registry assoc knob-id updated-knob)
      (set-bus-value! bus-id scaled-value)
      scaled-value)))

(defn get-knob [knob-id]
  (get @knob-registry knob-id))

;; =============================================================================
;; DEVICE ARCHITECTURE
;; =============================================================================

(defrecord Device [id type inputs outputs controls active?])

(defn create-device!
  "Create a new device and register it"
  [device-id device-type inputs outputs & {:keys [controls] :or {controls {}}}]
  (let [device (->Device device-id device-type inputs outputs controls true)]
    (swap! device-registry assoc device-id device)
    ;; Create buses for outputs
    (doseq [output-id outputs]
      (let [bus-id (keyword (str (name device-id) "-" (name output-id)))]
        (if (= output-id :audio-out)
          (create-audio-bus! bus-id)
          (create-control-bus! bus-id))))
    device))

(defn get-device [device-id]
  (get @device-registry device-id))

(defn connect-devices!
  "Connect output of source device to input of destination device"
  [source-device-id source-output dest-device-id dest-input]
  (let [connection-key [source-device-id source-output dest-device-id dest-input]
        source-bus-id (keyword (str (name source-device-id) "-" (name source-output)))]
    (swap! patch-matrix assoc connection-key source-bus-id)
    (println (str "Connected " source-device-id "/" source-output 
                  " → " dest-device-id "/" dest-input))))

(defn map-knob-to-device!
  "Map a knob to a device input with scaling"
  [knob-id device-id input-param & {:keys [min-val max-val curve]
                                    :or {min-val 0 max-val 1 curve :linear}}]
  ;; Update knob with proper scaling
  (when-let [knob (get-knob knob-id)]
    (let [updated-knob (assoc knob :min-val min-val :max-val max-val :curve curve)]
      (swap! knob-registry assoc knob-id updated-knob)))
  
  ;; Create connection in patch matrix
  (let [knob-bus-id (keyword (str "knob-" (name knob-id)))
        connection-key [:knob knob-id device-id input-param]]
    (swap! patch-matrix assoc connection-key knob-bus-id)
    (println (str "Mapped knob " knob-id " → " device-id "/" input-param 
                  " (range: " min-val " to " max-val ")"))))

;; =============================================================================
;; DEVICE IMPLEMENTATIONS
;; =============================================================================

(defn get-input-value
  "Get the current value for a device input (from knobs or other devices)"
  [device-id input-param default-value]
  (let [connection-key (some #(when (and (= (nth % 2) device-id) 
                                        (= (nth % 3) input-param)) %) 
                            (keys @patch-matrix))]
    (if connection-key
      (let [source-bus-id (get @patch-matrix connection-key)
            bus (get-bus source-bus-id)]
        (if bus (o/control-bus-get bus) default-value))
      default-value)))

;; Audio Oscillator Device
(o/defsynth audio-oscillator-synth
  [out-bus 0
   amplitude 0.5
   base-freq 440
   waveform 0
   osc2-offset 0
   osc3-offset 0
   detune 0
   portamento 0.1]
  (let [freq-slide (o/lag base-freq portamento)
        freq1 (+ freq-slide detune)
        freq2 (+ freq1 osc2-offset)
        freq3 (+ freq1 osc3-offset)
        
        osc1 (o/select:ar waveform [(o/saw freq1)
                                   (o/pulse freq1 0.5)
                                   (o/lf-tri freq1)
                                   (o/sin-osc freq1)])
        osc2 (o/select:ar waveform [(o/saw freq2)
                                   (o/pulse freq2 0.5)
                                   (o/lf-tri freq2)
                                   (o/sin-osc freq2)])
        osc3 (o/select:ar waveform [(o/saw freq3)
                                   (o/pulse freq3 0.5)
                                   (o/lf-tri freq3)
                                   (o/sin-osc freq3)])
        
        mixed (* amplitude (+ osc1 (* 0.5 osc2) (* 0.3 osc3)))]
    (o/out out-bus mixed)))

;; Low Frequency Oscillator Device  
(o/defsynth lfo-synth
  [out-bus 0
   frequency 2
   amplitude 1
   waveform 0
   offset 0.5]
  (let [lfo-sig (o/select:ar waveform [(o/sin-osc frequency)
                                      (o/lf-tri frequency)
                                      (o/lf-saw frequency)
                                      (o/lf-pulse frequency 0.5)])
        scaled-sig (+ offset (* amplitude lfo-sig))]
    (o/out out-bus scaled-sig)))

;; Filter Device
(o/defsynth filter-synth
  [in-bus 0
   out-bus 0  
   filter-type 0
   cutoff 1000
   resonance 0.3
   drive 1]
  (let [input-sig (* drive (o/in in-bus))
        filtered (o/select:ar filter-type [(o/moog-ff input-sig cutoff resonance)
                                          (o/hpf input-sig cutoff)
                                          (o/bpf input-sig cutoff resonance)])]
    (o/out out-bus filtered)))

;; Mixer Device
(o/defsynth mixer-synth
  [in-bus-1 0
   in-bus-2 0
   out-bus 0
   level-1 0.5
   level-2 0.5
   master-level 1]
  (let [sig1 (* level-1 (o/in in-bus-1))
        sig2 (* level-2 (o/in in-bus-2))
        mixed (* master-level (+ sig1 sig2))]
    (o/out out-bus mixed)))

;; Delay Device
(o/defsynth delay-synth
  [in-bus 0
   out-bus 0
   delay-time 0.25
   feedback 0.3
   mix 0.5]
  (let [input-sig (o/in in-bus)
        delayed (o/comb-c input-sig 2 delay-time feedback)
        mixed (+ (* (- 1 mix) input-sig) (* mix delayed))]
    (o/out out-bus mixed)))

;; Reverb Device
(o/defsynth reverb-synth
  [in-bus 0
   out-bus 0
   room-size 0.5
   damping 0.5
   mix 0.3]
  (let [input-sig (o/in in-bus)
        reverbed (o/free-verb input-sig room-size damping)
        mixed (+ (* (- 1 mix) input-sig) (* mix reverbed))]
    (o/out out-bus mixed)))

;; =============================================================================
;; DEVICE CREATION HELPERS
;; =============================================================================

(defn create-audio-oscillator! [device-id]
  (create-device! device-id :audio-osc 
                 [:amplitude :base-freq :waveform :osc2-offset :osc3-offset :detune :portamento]
                 [:audio-out]))

(defn create-lfo! [device-id] 
  (create-device! device-id :lfo
                 [:frequency :amplitude :waveform :offset]
                 [:cv-out]))

(defn create-filter! [device-id]
  (create-device! device-id :filter
                 [:audio-in :filter-type :cutoff :resonance :drive]
                 [:audio-out]))

(defn create-mixer! [device-id]
  (create-device! device-id :mixer
                 [:audio-in-1 :audio-in-2 :level-1 :level-2 :master-level]
                 [:audio-out]))

(defn create-delay! [device-id]
  (create-device! device-id :delay
                 [:audio-in :delay-time :feedback :mix]
                 [:audio-out]))

(defn create-reverb! [device-id]
  (create-device! device-id :reverb
                 [:audio-in :room-size :damping :mix]
                 [:audio-out]))

;; =============================================================================
;; PATCH PLAYBACK SYSTEM - SIMPLIFIED APPROACH
;; =============================================================================

(o/defsynth simple-modular-synth
  "Simplified modular synth with direct parameter control"
  [;; Oscillator controls
   osc-amp      {:default 0.5 :min 0 :max 1}
   osc-freq     {:default 1 :min 0.25 :max 4}  ; Multiplier for 440Hz
   osc-wave     {:default 0 :min 0 :max 3}
   osc2-offset  {:default 0 :min -12 :max 12}
   osc-detune   {:default 0 :min -50 :max 50}
   
   ;; LFO controls
   lfo-rate     {:default 2 :min 0.1 :max 20}
   lfo-depth    {:default 0 :min 0 :max 1}
   lfo-wave     {:default 0 :min 0 :max 3}
   lfo-target   {:default 0 :min 0 :max 2}  ; 0=amplitude, 1=frequency, 2=filter
   
   ;; Filter controls
   filter-cutoff {:default 0.5 :min 0.1 :max 1}
   filter-res    {:default 0.3 :min 0 :max 0.9}
   filter-type   {:default 0 :min 0 :max 2}
   
   ;; Effects
   delay-time   {:default 0.25 :min 0.01 :max 2}
   delay-mix    {:default 0 :min 0 :max 1}
   reverb-mix   {:default 0.2 :min 0 :max 1}
   
   ;; Global
   master-vol   {:default 0.6 :min 0 :max 1}]
   
  (let [;; LFO generation
        lfo-sig (o/select:ar lfo-wave [(o/sin-osc lfo-rate)
                                      (o/lf-tri lfo-rate)
                                      (o/lf-saw lfo-rate)
                                      (o/lf-pulse lfo-rate 0.5)])
        
        ;; LFO routing - target different parameters
        lfo-to-amp (* lfo-depth (o/select:ar lfo-target [lfo-sig 0 0]))
        lfo-to-freq (* lfo-depth 100 (o/select:ar lfo-target [0 lfo-sig 0]))
        lfo-to-filter (* lfo-depth 1000 (o/select:ar lfo-target [0 0 lfo-sig]))
        
        ;; Oscillator frequency calculation
        base-freq (* 440 osc-freq)
        mod-freq (+ base-freq osc-detune lfo-to-freq)
        osc2-freq (+ mod-freq osc2-offset)
        
        ;; Dual oscillators
        osc1 (o/select:ar osc-wave [(o/saw mod-freq)
                                   (o/pulse mod-freq 0.5)
                                   (o/lf-tri mod-freq)
                                   (o/sin-osc mod-freq)])
        
        osc2 (o/select:ar osc-wave [(o/saw osc2-freq)
                                   (o/pulse osc2-freq 0.5)
                                   (o/lf-tri osc2-freq)
                                   (o/sin-osc osc2-freq)])
        
        ;; Mix oscillators and apply amplitude modulation
        mixed-osc (* (+ osc1 (* 0.5 osc2)) 
                     osc-amp 
                     (+ 1 lfo-to-amp))
        
        ;; Filter with LFO modulation
        filter-freq (+ (* filter-cutoff 4000) lfo-to-filter)
        filtered (o/select:ar filter-type [(o/moog-ff mixed-osc filter-freq filter-res)
                                          (o/hpf mixed-osc filter-freq)
                                          (o/bpf mixed-osc filter-freq filter-res)])
        
        ;; Effects chain
        delayed (+ filtered 
                   (* delay-mix (o/comb-c filtered 2 delay-time 0.3)))
        
        reverbed (+ delayed
                    (* reverb-mix (o/free-verb delayed 0.5 0.5)))
        
        ;; Final output
        final-out (* reverbed master-vol)]
    
    (o/out 0 [final-out final-out])))

(defn start-patch! 
  "Start the patch playback system with direct parameter control"
  []
  (when @synth-node
    (o/kill @synth-node))
  (reset! synth-node (simple-modular-synth))
  (println "Simplified patch system started!"))

(defn update-synth-param! 
  "Update a synth parameter directly"
  [param value]
  (when @synth-node
    (o/ctl @synth-node param value)))

;; Enhanced knob system that directly controls synth parameters
(defn set-knob! 
  "Set knob value and directly update synth parameters"
  [knob-id value]
  (when-let [knob (get @knob-registry knob-id)]
    (let [clamped-value (max 0.0 (min 1.0 value))
          updated-knob (assoc knob :value clamped-value)
          scaled-value (scale-knob-value clamped-value 
                                        (:min-val knob) 
                                        (:max-val knob) 
                                        (:curve knob))
          bus-id (keyword (str "knob-" (name knob-id)))]
      (swap! knob-registry assoc knob-id updated-knob)
      (set-bus-value! bus-id scaled-value)
      
      ;; Direct synth parameter update mapping
      (let [param-map {:osc-amp :osc-amp
                       :osc-freq :osc-freq  
                       :osc-wave :osc-wave
                       :lfo-rate :lfo-rate
                       :lfo-depth :lfo-depth
                       :lfo-wave :lfo-wave
                       :lfo-target :lfo-target
                       :filter-cutoff :filter-cutoff
                       :filter-res :filter-res
                       :filter-type :filter-type
                       :delay-time :delay-time
                       :delay-mix :delay-mix
                       :reverb-mix :reverb-mix}
            synth-param (get param-map knob-id)]
        (when synth-param
          (update-synth-param! synth-param scaled-value)))
      
      scaled-value)))

(defn stop-patch!
  "Stop patch playback"
  []
  (when @synth-node
    (o/kill @synth-node)
    (reset! synth-node nil))
  (println "Patch system stopped!"))

;; =============================================================================
;; HIGH-LEVEL INTERFACE & PRESETS
;; =============================================================================

(defn setup-basic-synth!
  "Setup a basic synth patch with working real-time control"
  []
  ;; Create knobs with appropriate ranges
  (create-knob! :osc-amp :min-val 0 :max-val 1)
  (create-knob! :osc-freq :min-val 0.25 :max-val 4 :curve :exponential)  
  (create-knob! :osc-wave :min-val 0 :max-val 3)
  (create-knob! :lfo-rate :min-val 0.1 :max-val 20 :curve :exponential)
  (create-knob! :lfo-depth :min-val 0 :max-val 1)
  (create-knob! :lfo-target :min-val 0 :max-val 2)  ; LFO routing
  (create-knob! :filter-cutoff :min-val 0.1 :max-val 1 :curve :exponential)
  (create-knob! :filter-res :min-val 0 :max-val 0.9)
  (create-knob! :filter-type :min-val 0 :max-val 2)
  (create-knob! :delay-mix :min-val 0 :max-val 1)
  (create-knob! :reverb-mix :min-val 0 :max-val 1)
  
  ;; Set initial knob values
  (set-knob! :osc-amp 0.7)
  (set-knob! :osc-freq 0.5)       ; ~440 Hz
  (set-knob! :osc-wave 0)         ; Saw wave
  (set-knob! :lfo-rate 0.3)       ; ~2 Hz  
  (set-knob! :lfo-depth 0.0)      ; Start with no LFO
  (set-knob! :lfo-target 0)       ; LFO → amplitude
  (set-knob! :filter-cutoff 0.6)  ; ~1200 Hz
  (set-knob! :filter-res 0.3)
  (set-knob! :filter-type 0)      ; Lowpass
  (set-knob! :delay-mix 0)        ; No delay initially
  (set-knob! :reverb-mix 0.2)     ; Light reverb
  
  (println "Basic synth setup complete!")
  (println "Try: (set-knob! :lfo-depth 0.5) (set-knob! :lfo-target 1)"))

(defn setup-tremolo-example!
  "Setup the tremolo example with amplitude modulation"
  []
  (setup-basic-synth!)
  (set-knob! :lfo-rate 0.4)      ; 4 Hz tremolo
  (set-knob! :lfo-depth 0.8)     ; Deep amplitude modulation  
  (set-knob! :lfo-target 0)      ; LFO → amplitude
  (set-knob! :osc-wave 1)        ; Square wave
  (println "Tremolo patch ready!")
  (println "Knob controls: :osc-amp=volume, :lfo-rate=speed, :lfo-depth=intensity"))

(defn demo-knob-control!
  "Demonstrate real-time knob control with working audio"
  []
  (setup-basic-synth!)
  (start-patch!)
  (println "\nSynth is playing! Try these commands:")
  (println "(set-knob! :osc-freq 0.8)     ; Higher pitch") 
  (println "(set-knob! :lfo-depth 0.6)    ; Add tremolo")
  (println "(set-knob! :lfo-target 1)     ; LFO → frequency (vibrato)")
  (println "(set-knob! :filter-cutoff 0.2); Dark filter")
  (println "(set-knob! :osc-wave 2)       ; Triangle wave")
  (println "(set-knob! :delay-mix 0.4)    ; Add delay")
  (println "(stop-patch!)                 ; Stop audio"))

(defn show-patch-status []
  "Display current patch matrix and knob settings"
  (println "\n=== KNOBS ===")
  (doseq [[id knob] @knob-registry]
    (println (str id ": " (:value knob) 
                  " → " (scale-knob-value (:value knob) (:min-val knob) (:max-val knob) (:curve knob)))))
  
  (println "\n=== DEVICES ===")
  (doseq [[id device] @device-registry]
    (println (str id " (" (:type device) "): " 
                  "inputs=" (:inputs device) 
                  " outputs=" (:outputs device))))
  
  (println "\n=== CONNECTIONS ===")
  (doseq [[connection bus] @patch-matrix]
    (println (str connection " via bus " bus))))

;; =============================================================================
;; CLEANUP UTILITIES
;; =============================================================================

(defn reset-patch-matrix!
  "Clear all devices, knobs, and connections"
  []
  (stop-patch!)
  (reset! patch-matrix {})
  (reset! device-registry {})
  (reset! knob-registry {})
  (reset! bus-registry {})
  (println "Patch matrix reset!"))

(comment
  ;; =============================================================================
  ;; USAGE EXAMPLES & WORKFLOWS
  ;; =============================================================================
  
  ;; Quick start - basic synth
  (setup-basic-synth!)
  (start-patch!)
  
  ;; Real-time knob control
  (set-knob! :osc-freq 0.4)      ; Higher pitch
  (set-knob! :lfo-rate 0.9)      ; Faster tremolo  
  (set-knob! :filter-cutoff 0.2) ; Darker sound
  (set-knob! :osc-wave 2)        ; Triangle wave
  
  ;; Try the tremolo example
  (setup-tremolo-example!)
  (start-patch!)
  
  ;; Check what's happening
  (show-patch-status)
  
  ;; Manual device creation and patching
  (reset-patch-matrix!)
  (create-knob! :freq-knob :min-val 220 :max-val 880)
  (create-knob! :amp-knob :min-val 0 :max-val 1)
  (create-audio-oscillator! :osc1)
  (map-knob-to-device! :freq-knob :osc1 :base-freq :min-val 220 :max-val 880)
  (map-knob-to-device! :amp-knob :osc1 :amplitude :min-val 0 :max-val 1)

  
  ;; Control it
  (set-knob! :freq-knob 0.8)     ; High pitch
  (set-knob! :amp-knob 0.6)      ; Medium volume
  
  ;; Stop everything
  (stop-patch!)
  (reset-patch-matrix!)
)
