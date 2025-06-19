(ns megasynth.claude4-v14)

;; =============================================================================
;; HIGH-LEVEL INTERFACE & EXAMPLES  
;; =============================================================================

(defn demo-modular-patch!
  "Demonstrate a complete modular patch: Osc → Filter → Output"
  []
  (println "Creating modular patch: Oscillator → Filter → Output")
  
  ;; Create devices
  (create-audio-oscillator! :osc1)
  (create-filter! :filter1) 
  (create-output! :main-out)
  
  ;; Set up audio routing
  (connect-audio! :osc1 0 :filter1 0)    ; Osc output → Filter input
  (connect-audio! :filter1 1 :main-out 0) ; Filter output → Main output
  
  ;; Create knobs for control
  (create-knob! :osc-freq :min-val 220 :max-val 880)
  (create-knob! :osc-amp :min-val 0 :max-val 1)
  (create-knob! :filter-cutoff :min-val 200 :max-val 4000 :curve :exponential)
  (create-knob! :filter-res :min-val 0 :max-val 0.9)
  (create-knob! :output-level :min-val 0 :max-val 1)
  
  ;; Map knobs to device parameters
  (map-knob-to-device-param! :osc-freq :osc1 1 :min-val 220 :max-val 880)      ; freq
  (map-knob-to-device-param! :osc-amp :osc1 0 :min-val 0 :max-val 1)           ; amplitude  
  (map-knob-to-device-param! :filter-cutoff :filter1 1 :min-val 200 :max-val 4000) ; cutoff
  (map-knob-to-device-param! :filter-res :filter1 2 :min-val 0 :max-val 0.9)   ; resonance
  (map-knob-to-device-param! :output-level :main-out 0 :min-val 0 :max-val 1)  ; output level
  
  ;; Set initial values
  (set-knob! :osc-freq 0.5)      ; 440 Hz
  (set-knob! :osc-amp 0.7)       ; 70% amplitude
  (set-knob! :filter-cutoff 0.6) ; ~1200 Hz cutoff
  (set-knob! :filter-res 0.3)    ; 30% resonance  
  (set-knob! :output-level 0.6)  ; 60% output level
  
  (println "\nModular patch ready! Try:")
  (println "(set-knob! :osc-freq 0.8)      ; Higher pitch")
  (println "(set-knob! :filter-cutoff 0.2) ; Dark filter") 
  (println "(set-knob! :filter-res 0.7)    ; More resonance")
  (println "(show-patch-status)            ; See connections"))

(defn demo-tremolo-patch!
  "Demonstrate tremolo using LFO to modulate oscillator amplitude"
  []
  (println "Creating tremolo patch: LFO modulating oscillator amplitude")
  
  ;; Create devices
  (create-lfo! :lfo1)
  (create-audio-oscillator! :osc1)
  (create-output! :main-out)
  
  ;; Route audio: Osc → Output
  (connect-audio! :osc1 0 :main-out 0)
  
  ;; Create knobs
  (create-knob! :osc-freq :min-val 220 :max-val 880)
  (create-knob! :base-amp :min-val 0.2 :max-val 0.8)    ; Base amplitude
  (create-knob! :lfo-rate :min-val 0.5 :max-val 10)     ; Tremolo speed
  (create-knob! :lfo-depth :min-val 0 :max-val 0.5)     ; Tremolo depth
  (create-knob! :output-level :min-val 0 :max-val 1)
  
  ;; Map knobs to parameters
  (map-knob-to-device-param! :osc-freq :osc1 1 :min-val 220 :max-val 880)
  (map-knob-to-device-param! :base-amp :osc1 0 :min-val 0.2 :max-val 0.8)
  (map-knob-to-device-param! :lfo-rate :lfo1 0 :min-val 0.5 :max-val 10)
  (map-knob-to-device-param! :lfo-depth :lfo1 1 :min-val 0 :max-val 0.5)
  (map-knob-to-device-param! :output-level :main-out 0 :min-val 0 :max-val 1)
  
  ;; Set LFO to create tremolo effect
  (set-device-param! :lfo1 3 0.5)  ; LFO offset = 0.5 (so it oscillates around 0.5)
  
  ;; Set initial values
  (set-knob! :osc-freq 0.5)      ; 440 Hz
  (set-knob! :base-amp 0.6)      ; Base volume
  (set-knob! :lfo-rate 0.3)      ; ~2 Hz tremolo
  (set-knob! :lfo-depth 0.4)     ; Moderate tremolo depth
  (set-knob! :output-level 0.7)  ; Output level
  
  (println "\nTremolo patch ready! Try:")
  (println "(set-knob! :lfo-rate 0.8)   ; Faster tremolo")
  (println "(set-knob! :lfo-depth 0.8)  ; Deeper tremolo")
  (println "(set-knob! :osc-freq 0.3)   ; Lower pitch"))

(defn demo-complex-patch!
  "Demonstrate a complex patch: Dual Osc → Mixer → Filter → Delay → Output"
  []
  (println "Creating complex patch: Dual Oscillators → Mixer → Filter → Delay → Output")
  
  ;; Create all devices
  (create-audio-oscillator! :osc1)
  (create-audio-oscillator! :osc2)
  (create-mixer! :mixer1)
  (create-filter! :filter1)
  (create-delay! :delay1)
  (create-output! :main-out)
  
  ;; Set up audio routing chain
  (connect-audio! :osc1 0 :mixer1 0)     ; Osc1 → Mixer input 1
  (connect-audio! :osc2 0 :mixer1 1)     ; Osc2 → Mixer input 2  
  (connect-audio! :mixer1 2 :filter1 0)  ; Mixer output → Filter input
  (connect-audio! :filter1 1 :delay1 0)  ; Filter output → Delay input
  (connect-audio! :delay1 1 :main-out 0) ; Delay output → Main output
  
  ;; Create knobs for comprehensive control
  (create-knob! :osc1-freq :min-val 220 :max-val 880)
  (create-knob! :osc2-freq :min-val 220 :max-val 880)
  (create-knob! :osc1-level :min-val 0 :max-val 1)
  (create-knob! :osc2-level :min-val 0 :max-val 1)
  (create-knob! :filter-cutoff :min-val 200 :max-val 4000 :curve :exponential)
  (create-knob! :filter-res :min-val 0 :max-val 0.9)
  (create-knob! :delay-time :min-val 0.05 :max-val 1)
  (create-knob! :delay-mix :min-val 0 :max-val 0.8)
  (create-knob! :output-level :min-val 0 :max-val 1)
  
  ;; Map knobs to device parameters
  (map-knob-to-device-param! :osc1-freq :osc1 1 :min-val 220 :max-val 880)
  (map-knob-to-device-param! :osc2-freq :osc2 1 :min-val 220 :max-val 880)
  (map-knob-to-device-param! :osc1-level :mixer1 0 :min-val 0 :max-val 1)
  (map-knob-to-device-param! :osc2-level :mixer1 1 :min-val 0 :max-val 1)
  (map-knob-to-device-param! :filter-cutoff :filter1 1 :min-val 200 :max-val 4000)
  (map-knob-to-device-param! :filter-res :filter1 2 :min-val 0 :max-val 0.9)
  (map-knob-to-device-param! :delay-time :delay1 0 :min-val 0.05 :max-val 1)
  (map-knob-to-device-param! :delay-mix :delay1 2 :min-val 0 :max-val 0.8)
  (map-knob-to-device-param! :output-level :main-out 0 :min-val 0 :max-val 1)
  
  ;; Set initial patch values
  (set-knob! :osc1-freq 0.5)     ; 440 Hz
  (set-knob! :osc2-freq 0.52)    ; Slightly detuned
  (set-knob! :osc1-level 0.7)    ; Osc1 level
  (set-knob! :osc2-level 0.5)    ; Osc2 level  
  (set-knob! :filter-cutoff 0.6) ; Medium filter
  (set-knob! :filter-res 0.4)    ; Some resonance
  (set-knob! :delay-time 0.3)    ; 375ms delay
  (set-knob! :delay-mix 0.3)     ; Light delay mix
  (set-knob! :output-level 0.6)  ; Output level
  
  ;; Set mixer master level
  (set-device-param! :mixer1 2 0.8)  ; Mixer master level
  
  ;; Set oscillator waveforms
  (set-device-param! :osc1 2 0)      ; Osc1 = saw wave
  (set-device-param! :osc2 2 1)      ; Osc2 = square wave
  
  (println "\nComplex patch ready! Try:")
  (println "(set-knob! :osc2-freq 0.6)     ; Detune osc2 more")
  (println "(set-knob! :delay-time 0.8)    ; Longer delay")
  (println "(set-knob! :filter-res 0.8)    ; Screaming filter")
  (println "(set-knob! :delay-mix 0.6)     ; More delay effect"))

;; =============================================================================
;; UTILITY & STATUS FUNCTIONS
;; =============================================================================

(defn show-patch-status []
  "Display current patch matrix, devices, and knob settings"
  (println "\n=== KNOBS ===")
  (doseq [[id knob] @knob-registry]
    (println (str id ": " (:value knob) 
                  " → " (scale-knob-value (:value knob) (:min-val knob) (:max-val knob) (:curve knob)))))
  
  (println "\n=== DEVICES ===")
  (doseq [[id device] @device-registry]
    (println (str id " (" (:type device) "): " 
                  "inputs=" (:inputs device) 
                  " outputs=" (:outputs device))))
  
  (println "\n=== RUNNING SYNTHS ===")
  (doseq [[id synth] @device-synths]
    (println (str id ": " synth)))
  
  (println "\n=== AUDIO CONNECTIONS ===")
  (doseq [[connection router] @connection-matrix]
    (println (str (first connection) "[" (second connection) "] → " 
                  (nth connection 2) "[" (nth connection 3) "]")))
  
  (println "\n=== KNOB MAPPINGS ===")
  (doseq [[connection info] @patch-matrix]
    (when (= (first connection) :knob)
      (println (str (second connection) " → " (:device-id info) " param[" (:param-index info) "]")))))

(defn stop-all-devices!
  "Stop all running devices and clear the patch"
  []
  (doseq [[device-id _] @device-synths]
    (stop-device! device-id))
  (doseq [[_ router-synth] @connection-matrix]
    (o/kill router-synth))
  (reset! device-registry {})
  (reset! device-synths {})
  (reset! device-buses {})
  (reset! connection-matrix {})
  (reset! knob-registry {})
  (reset! patch-matrix {})
  (println "All devices stopped and patch cleared!"))

;; =============================================================================
;; CLEANUP UTILITIES
;; =============================================================================

(defn reset-patch-matrix!
  "Clear all devices, knobs, and connections"
  []
  (stop-all-devices!)
  (reset! bus-registry {})
  (println "Patch matrix completely reset!"))(ns diy-synth.modular
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
;; DEVICE IMPLEMENTATIONS - ACTUAL RUNNING SYNTHS
;; =============================================================================

;; Audio Oscillator Device - Creates actual running synth
(o/defsynth audio-oscillator-device
  [out-bus 0
   amp-bus 20      ; Control bus for amplitude
   freq-bus 21     ; Control bus for frequency  
   wave-bus 22     ; Control bus for waveform
   detune-bus 23   ; Control bus for detune
   osc2-offset-bus 24  ; Control bus for osc2 offset
   portamento 0.1]
  (let [amplitude (o/in:kr amp-bus)
        base-freq (o/in:kr freq-bus)
        waveform (o/in:kr wave-bus)
        detune (o/in:kr detune-bus)
        osc2-offset (o/in:kr osc2-offset-bus)
        
        freq-slide (o/lag base-freq portamento)
        freq1 (+ freq-slide detune)
        freq2 (+ freq1 osc2-offset)
        
        osc1 (o/select:ar waveform [(o/saw freq1)
                                   (o/pulse freq1 0.5)
                                   (o/lf-tri freq1)
                                   (o/sin-osc freq1)])
        osc2 (o/select:ar waveform [(o/saw freq2)
                                   (o/pulse freq2 0.5)
                                   (o/lf-tri freq2)
                                   (o/sin-osc freq2)])
        
        mixed (* amplitude (+ osc1 (* 0.5 osc2)))]
    (o/out out-bus mixed)))

;; Low Frequency Oscillator Device
(o/defsynth lfo-device
  [out-bus 0
   freq-bus 20    ; Control bus for frequency
   amp-bus 21     ; Control bus for amplitude
   wave-bus 22    ; Control bus for waveform
   offset-bus 23] ; Control bus for offset
  (let [frequency (o/in:kr freq-bus)
        amplitude (o/in:kr amp-bus)
        waveform (o/in:kr wave-bus)
        offset (o/in:kr offset-bus)
        
        lfo-sig (o/select:ar waveform [(o/sin-osc frequency)
                                      (o/lf-tri frequency)
                                      (o/lf-saw frequency)
                                      (o/lf-pulse frequency 0.5)])
        scaled-sig (+ offset (* amplitude lfo-sig))]
    (o/out out-bus scaled-sig)))

;; Filter Device
(o/defsynth filter-device
  [in-bus 0
   out-bus 0
   type-bus 20    ; Control bus for filter type
   cutoff-bus 21  ; Control bus for cutoff
   res-bus 22     ; Control bus for resonance
   drive-bus 23]  ; Control bus for drive
  (let [input-sig (o/in in-bus)
        filter-type (o/in:kr type-bus)
        cutoff (o/in:kr cutoff-bus)
        resonance (o/in:kr res-bus)
        drive (o/in:kr drive-bus)
        
        driven-sig (* drive input-sig)
        filtered (o/select:ar filter-type [(o/moog-ff driven-sig cutoff resonance)
                                          (o/hpf driven-sig cutoff)
                                          (o/bpf driven-sig cutoff resonance)])]
    (o/out out-bus filtered)))

;; Mixer Device
(o/defsynth mixer-device
  [in-bus-1 0
   in-bus-2 0
   out-bus 0
   level1-bus 20  ; Control bus for level 1
   level2-bus 21  ; Control bus for level 2
   master-bus 22] ; Control bus for master level
  (let [sig1 (o/in in-bus-1)
        sig2 (o/in in-bus-2)
        level1 (o/in:kr level1-bus)
        level2 (o/in:kr level2-bus)
        master (o/in:kr master-bus)
        
        mixed (* master (+ (* level1 sig1) (* level2 sig2)))]
    (o/out out-bus mixed)))

;; Delay Device
(o/defsynth delay-device
  [in-bus 0
   out-bus 0
   time-bus 20    ; Control bus for delay time
   feedback-bus 21 ; Control bus for feedback
   mix-bus 22]    ; Control bus for mix
  (let [input-sig (o/in in-bus)
        delay-time (o/in:kr time-bus)
        feedback (o/in:kr feedback-bus)
        mix (o/in:kr mix-bus)
        
        delayed (o/comb-c input-sig 2 delay-time feedback)
        mixed (+ (* (- 1 mix) input-sig) (* mix delayed))]
    (o/out out-bus mixed)))

;; Reverb Device
(o/defsynth reverb-device
  [in-bus 0
   out-bus 0
   room-bus 20    ; Control bus for room size
   damp-bus 21    ; Control bus for damping
   mix-bus 22]    ; Control bus for mix
  (let [input-sig (o/in in-bus)
        room-size (o/in:kr room-bus)
        damping (o/in:kr damp-bus)
        mix (o/in:kr mix-bus)
        
        reverbed (o/free-verb input-sig room-size damping)
        mixed (+ (* (- 1 mix) input-sig) (* mix reverbed))]
    (o/out out-bus mixed)))

;; Output Device - Routes signals to main audio output
(o/defsynth output-device
  [in-bus 0
   level-bus 20   ; Control bus for output level
   pan-bus 21]    ; Control bus for panning
  (let [input-sig (o/in in-bus)
        level (o/in:kr level-bus)
        pan (o/in:kr pan-bus)
        
        output-sig (* input-sig level)]
    (o/out 0 (o/pan2 output-sig pan))))

;; =============================================================================
;; ENHANCED DEVICE MANAGEMENT
;; =============================================================================

(defonce device-synths (atom {}))     ; Running synth instances
(defonce device-buses (atom {}))      ; Bus allocations per device
(defonce connection-matrix (atom {})) ; Audio routing connections

(defn allocate-device-buses!
  "Allocate control and audio buses for a device"
  [device-id device-type]
  (let [control-buses (repeatedly 10 o/control-bus)  ; Allocate 10 control buses per device
        audio-buses (case device-type
                      :audio-osc [(o/audio-bus)]           ; 1 audio output
                      :lfo [(o/control-bus)]               ; 1 control output (CV)
                      :filter [(o/audio-bus) (o/audio-bus)] ; input + output
                      :mixer [(o/audio-bus) (o/audio-bus) (o/audio-bus)] ; 2 inputs + 1 output
                      :delay [(o/audio-bus) (o/audio-bus)]  ; input + output
                      :reverb [(o/audio-bus) (o/audio-bus)] ; input + output
                      :output [(o/audio-bus)]               ; 1 input
                      [])]
    (swap! device-buses assoc device-id {:control control-buses :audio audio-buses})
    {:control control-buses :audio audio-buses}))

(defn get-device-bus
  "Get a specific bus for a device"
  [device-id bus-type index]
  (get-in @device-buses [device-id bus-type index]))

(defn start-device!
  "Start a device synth with allocated buses"
  [device-id device-type]
  (let [buses (allocate-device-buses! device-id device-type)
        control-buses (:control buses)
        audio-buses (:audio buses)]
    
    ;; Initialize control buses with default values
    (doseq [[i default-val] (map-indexed vector [0.5 440 0 0 0 0.5 1000 0.3 1 0.5])]
      (o/control-bus-set! (nth control-buses i) default-val))
    
    (let [synth-instance
          (case device-type
            :audio-osc (audio-oscillator-device 
                       :out-bus (first audio-buses)
                       :amp-bus (nth control-buses 0)
                       :freq-bus (nth control-buses 1)
                       :wave-bus (nth control-buses 2)
                       :detune-bus (nth control-buses 3)
                       :osc2-offset-bus (nth control-buses 4))
            
            :lfo (lfo-device
                 :out-bus (first audio-buses)
                 :freq-bus (nth control-buses 0)
                 :amp-bus (nth control-buses 1)
                 :wave-bus (nth control-buses 2)
                 :offset-bus (nth control-buses 3))
            
            :filter (filter-device
                    :in-bus (first audio-buses)
                    :out-bus (second audio-buses)
                    :type-bus (nth control-buses 0)
                    :cutoff-bus (nth control-buses 1)
                    :res-bus (nth control-buses 2)
                    :drive-bus (nth control-buses 3))
            
            :mixer (mixer-device
                   :in-bus-1 (first audio-buses)
                   :in-bus-2 (second audio-buses)
                   :out-bus (nth audio-buses 2)
                   :level1-bus (nth control-buses 0)
                   :level2-bus (nth control-buses 1)
                   :master-bus (nth control-buses 2))
            
            :delay (delay-device
                   :in-bus (first audio-buses)
                   :out-bus (second audio-buses)
                   :time-bus (nth control-buses 0)
                   :feedback-bus (nth control-buses 1)
                   :mix-bus (nth control-buses 2))
            
            :reverb (reverb-device
                    :in-bus (first audio-buses)
                    :out-bus (second audio-buses)
                    :room-bus (nth control-buses 0)
                    :damp-bus (nth control-buses 1)
                    :mix-bus (nth control-buses 2))
            
            :output (output-device
                    :in-bus (first audio-buses)
                    :level-bus (nth control-buses 0)
                    :pan-bus (nth control-buses 1)))]
      
      (swap! device-synths assoc device-id synth-instance)
      (println (str "Started device " device-id " (" device-type ")"))
      synth-instance)))

(defn stop-device!
  "Stop a device synth and free its buses"
  [device-id]
  (when-let [synth-instance (get @device-synths device-id)]
    (o/kill synth-instance)
    (swap! device-synths dissoc device-id)
    ;; Note: In a production system, you'd want to free the buses here
    (println (str "Stopped device " device-id))))

(defn set-device-param!
  "Set a parameter on a device via its control bus"
  [device-id param-index value]
  (when-let [control-bus (get-device-bus device-id :control param-index)]
    (o/control-bus-set! control-bus value)
    (println (str "Set " device-id " param " param-index " = " value))))

;; =============================================================================
;; AUDIO ROUTING SYSTEM
;; =============================================================================

(defn connect-audio!
  "Connect audio output of source device to input of destination device"
  [source-device-id source-output-index dest-device-id dest-input-index]
  (let [source-bus (get-device-bus source-device-id :audio source-output-index)
        dest-bus (get-device-bus dest-device-id :audio dest-input-index)]
    (when (and source-bus dest-bus)
      ;; Create a simple pass-through synth to route the audio
      (let [router-synth (o/defsynth audio-router []
                          (o/out dest-bus (o/in source-bus)))]
        (swap! connection-matrix assoc [source-device-id source-output-index dest-device-id dest-input-index] 
               (router-synth))
        (println (str "Connected " source-device-id "[" source-output-index "] → " 
                     dest-device-id "[" dest-input-index "]"))))))

(defn disconnect-audio!
  "Disconnect an audio routing"
  [source-device-id source-output-index dest-device-id dest-input-index]
  (let [connection-key [source-device-id source-output-index dest-device-id dest-input-index]]
    (when-let [router-synth (get @connection-matrix connection-key)]
      (o/kill router-synth)
      (swap! connection-matrix dissoc connection-key)
      (println (str "Disconnected " source-device-id "[" source-output-index "] ↛ " 
                   dest-device-id "[" dest-input-index "]")))))

;; =============================================================================
;; ENHANCED DEVICE CREATION HELPERS
;; =============================================================================

(defn create-audio-oscillator! 
  "Create and start an audio oscillator device"
  [device-id]
  (create-device! device-id :audio-osc 
                 [:amplitude :base-freq :waveform :detune :osc2-offset]
                 [:audio-out])
  (start-device! device-id :audio-osc))

(defn create-lfo! 
  "Create and start an LFO device"
  [device-id] 
  (create-device! device-id :lfo
                 [:frequency :amplitude :waveform :offset]
                 [:cv-out])
  (start-device! device-id :lfo))

(defn create-filter! 
  "Create and start a filter device"
  [device-id]
  (create-device! device-id :filter
                 [:audio-in :filter-type :cutoff :resonance :drive]
                 [:audio-out])
  (start-device! device-id :filter))

(defn create-mixer! 
  "Create and start a mixer device"
  [device-id]
  (create-device! device-id :mixer
                 [:audio-in-1 :audio-in-2 :level-1 :level-2 :master-level]
                 [:audio-out])
  (start-device! device-id :mixer))

(defn create-delay! 
  "Create and start a delay device"
  [device-id]
  (create-device! device-id :delay
                 [:audio-in :delay-time :feedback :mix]
                 [:audio-out])
  (start-device! device-id :delay))

(defn create-reverb! 
  "Create and start a reverb device"
  [device-id]
  (create-device! device-id :reverb
                 [:audio-in :room-size :damping :mix]
                 [:audio-out])
  (start-device! device-id :reverb))

(defn create-output! 
  "Create and start an output device"
  [device-id]
  (create-device! device-id :output
                 [:audio-in :level :pan]
                 [])
  (start-device! device-id :output))

;; =============================================================================
;; ENHANCED KNOB TO DEVICE PARAMETER MAPPING
;; =============================================================================

(defn map-knob-to-device-param!
  "Map a knob to a specific device parameter with real control bus updates"
  [knob-id device-id param-index & {:keys [min-val max-val curve]
                                    :or {min-val 0 max-val 1 curve :linear}}]
  ;; Update knob with proper scaling
  (when-let [knob (get-knob knob-id)]
    (let [updated-knob (assoc knob :min-val min-val :max-val max-val :curve curve)]
      (swap! knob-registry assoc knob-id updated-knob)))
  
  ;; Create connection in patch matrix
  (let [connection-key [:knob knob-id device-id param-index]]
    (swap! patch-matrix assoc connection-key {:device-id device-id :param-index param-index})
    (println (str "Mapped knob " knob-id " → " device-id " param[" param-index "] " 
                  " (range: " min-val " to " max-val ")"))))

;; Enhanced knob system that updates device parameters via control buses
(defn set-knob! 
  "Set knob value and update connected device parameters via control buses"
  [knob-id value]
  (when-let [knob (get @knob-registry knob-id)]
    (let [clamped-value (max 0.0 (min 1.0 value))
          updated-knob (assoc knob :value clamped-value)
          scaled-value (scale-knob-value clamped-value 
                                        (:min-val knob) 
                                        (:max-val knob) 
                                        (:curve knob))]
      (swap! knob-registry assoc knob-id updated-knob)
      
      ;; Update all connected device parameters
      (doseq [[connection-key connection-info] @patch-matrix]
        (when (and (= (first connection-key) :knob)
                   (= (second connection-key) knob-id))
          (let [{:keys [device-id param-index]} connection-info]
            (set-device-param! device-id param-index scaled-value))))
      
      scaled-value)))

;; =============================================================================
;; DEVICE CREATION HELPERS
;; =============================================================================

(defn create-audio-oscillator! 
  "Create and start an audio oscillator device"
  [device-id]
  (create-device! device-id :audio-osc 
                 [:amplitude :base-freq :waveform :detune :osc2-offset]
                 [:audio-out])
  (start-device! device-id :audio-osc))

(defn create-lfo! 
  "Create and start an LFO device"
  [device-id] 
  (create-device! device-id :lfo
                 [:frequency :amplitude :waveform :offset]
                 [:cv-out])
  (start-device! device-id :lfo))

(defn create-filter! 
  "Create and start a filter device"
  [device-id]
  (create-device! device-id :filter
                 [:audio-in :filter-type :cutoff :resonance :drive]
                 [:audio-out])
  (start-device! device-id :filter))

(defn create-mixer! 
  "Create and start a mixer device"
  [device-id]
  (create-device! device-id :mixer
                 [:audio-in-1 :audio-in-2 :level-1 :level-2 :master-level]
                 [:audio-out])
  (start-device! device-id :mixer))

(defn create-delay! 
  "Create and start a delay device"
  [device-id]
  (create-device! device-id :delay
                 [:audio-in :delay-time :feedback :mix]
                 [:audio-out])
  (start-device! device-id :delay))

(defn create-reverb! 
  "Create and start a reverb device"
  [device-id]
  (create-device! device-id :reverb
                 [:audio-in :room-size :damping :mix]
                 [:audio-out])
  (start-device! device-id :reverb))

(defn create-output! 
  "Create and start an output device"
  [device-id]
  (create-device! device-id :output
                 [:audio-in :level :pan]
                 [])
  (start-device! device-id :output))

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
  (set-knob! :osc-freq 0.8)      ; Higher pitch
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
