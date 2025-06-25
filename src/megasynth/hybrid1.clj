(ns megasynth.hybrid1
  (:require [overtone.live :as o]))

;; =============================================================================
;; MISC
;; =============================================================================

(defn call-stack
  "Returns a vector of the closest `n` stack frames as maps containing class, method, file, and line."
  [drop-n take-n]
  (let [stack (.getStackTrace (Thread/currentThread))]
    (->> stack
         (drop drop-n)
         (take take-n)
         (map (fn [^StackTraceElement el]
                {:class (.getClassName el)
                 :method (.getMethodName el)
                 :file (.getFileName el)
                 :line (.getLineNumber el)}))
         vec)))

(declare get-device-bus)
(declare device-buses)

(defn ->print-str [& x]
  (with-out-str
    (apply clojure.pprint/pprint x)))

(defn cap-out [bus signals]
  (let [r (o/out bus signals)
        lines (mapv :line (call-stack 5 2))]
    (println (apply format "==> OUT
 return: %s bus: %s signals: %s lines: %s
-----
"
                    (map ->print-str [r bus signals lines])))
    r))



(defn synth-destroyed? [s]
  (-> s .status deref (= :destroyed)))

(defn monitor-device! [device-id output-index]
  (let [bus (get-device-bus device-id :audio output-index)]
    (o/defsynth tap-monitor [in-bus 0]
      (cap-out 0 (* 0.5 (o/in in-bus))))
    (tap-monitor :in-bus (.id bus))
    (println (str "Tapping output from " device-id " output[" output-index "]"))))



(defn set-device-control-bus! [device-id ctrl-index value]
  (let [bus (get-in @device-buses [device-id :control ctrl-index])]
    (if bus
      (do (o/control-bus-set! bus value)
          (println (str "Set control bus " ctrl-index " of " device-id " to " value)))
      (println "No control bus at that index."))))


;; =============================================================================
;; GLOBAL STATE - DEFINED FIRST
;; =============================================================================

(defonce patch-matrix (atom {}))
(defonce device-registry (atom {}))
(defonce knob-registry (atom {}))
#_(defonce bus-registry (atom {}))
(defonce synth-node (atom nil))
(defonce device-synths (atom {}))
(defonce device-buses (atom {}))
(defonce connection-matrix (atom {}))

;; =============================================================================
;; BUS MANAGEMENT
;; =============================================================================

(comment
  ;; Claude garbage
  
  (defn create-control-bus!
    "Create a control rate bus for CV signals"
    [bus-id]
    (let [bus (o/control-bus)]
      (swap! bus-registry assoc bus-id bus)
      (o/control-bus-set! bus 0)
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

  (comment))

;; =============================================================================
;; KNOB SYSTEM (0.0 - 1.0 normalized values)
;; =============================================================================

(defrecord Knob [id value min-val max-val curve])

(defn scale-knob-value
  "Convert 0.0-1.0 knob value to actual parameter range"
  [knob-value min-val max-val curve]
  (case curve
    :linear (+ min-val (* knob-value (- max-val min-val)))
    :exponential (+ min-val (* (Math/pow knob-value 2) (- max-val min-val)))
    :logarithmic (+ min-val (* (Math/sqrt knob-value) (- max-val min-val)))
    (+ min-val (* knob-value (- max-val min-val)))))

(defn create-knob! 
  "Create a knob with 0.0-1.0 range that maps to actual parameter ranges"
  [knob-id & {:keys [min-val max-val curve] 
              :or {min-val 0 max-val 1 curve :linear}}]
  (let [knob (->Knob knob-id 0.5 min-val max-val curve)]
    (swap! knob-registry assoc knob-id knob)
    #_(create-control-bus! (keyword (str "knob-" (name knob-id))))
    knob))

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
    #_(doseq [output-id outputs]
      (let [bus-id (keyword (str (name device-id) "-" (name output-id)))]
        (if (= output-id :audio-out)
          (create-audio-bus! bus-id)
          (create-control-bus! bus-id))))
    device))

(defn get-device [device-id]
  (get @device-registry device-id))

;; =============================================================================
;; DEVICE SYNTH IMPLEMENTATIONS
;; =============================================================================

(o/defsynth audio-oscillator-device
  [out-bus 0
   amp-bus 20
   freq-bus 21
   wave-bus 22
   detune-bus 23
   osc2-offset-bus 24
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
    (cap-out out-bus mixed)))

(o/defsynth lfo-device
  [out-bus 0
   freq-bus 20
   amp-bus 21
   wave-bus 22
   offset-bus 23]
  (let [frequency (o/in:kr freq-bus)
        amplitude (o/in:kr amp-bus)
        waveform (o/in:kr wave-bus)
        offset (o/in:kr offset-bus)
        
        lfo-sig (o/select:ar waveform [(o/sin-osc frequency)
                                      (o/lf-tri frequency)
                                      (o/lf-saw frequency)
                                      (o/lf-pulse frequency 0.5)])
        scaled-sig (+ offset (* amplitude lfo-sig))]
    (cap-out out-bus scaled-sig)))

#_
(o/defsynth filter-device
  [in-bus 0
   out-bus 0
   type-bus 20
   cutoff-bus 21
   res-bus 22
   drive-bus 23]
  (let [input-sig (o/in in-bus)
        filter-type (o/in:kr type-bus)
        cutoff (o/in:kr cutoff-bus)
        resonance (o/in:kr res-bus)
        drive (o/in:kr drive-bus)
        driven-sig (* drive input-sig)
        filtered (o/select:ar filter-type [(o/moog-ff driven-sig cutoff resonance)
                                          (o/hpf driven-sig cutoff)
                                          (o/bpf driven-sig cutoff resonance)])]
    (cap-out out-bus filtered)))

#_(o/defsynth filter-device
  [in-bus 0
   out-bus 0
   cutoff-bus 21]
  (let [sig (o/in in-bus)
        cutoff (o/in:kr cutoff-bus)
        filtered (o/rlpf sig cutoff 0.5)]
    (cap-out out-bus filtered)))

#_(o/defsynth filter-device
  [in-bus 0
   out-bus 0]
  (let [sig (o/in in-bus)
        filtered (o/rlpf sig 0 0.5)] ; fixed cutoff
    (cap-out out-bus filtered)))

(o/defsynth filter-device
  [in-bus 0
   out-bus 0]
  (let [sig (o/in in-bus)
        filtered 0.0]
    (cap-out out-bus sig)))

(o/defsynth mixer-device
  [in-bus-1 0
   in-bus-2 0
   out-bus 0
   level1-bus 20
   level2-bus 21
   master-bus 22]
  (let [sig1 (o/in in-bus-1)
        sig2 (o/in in-bus-2)
        level1 (o/in:kr level1-bus)
        level2 (o/in:kr level2-bus)
        master (o/in:kr master-bus)
        
        mixed (* master (+ (* level1 sig1) (* level2 sig2)))]
    (cap-out out-bus mixed)))

(o/defsynth delay-device
  [in-bus 0
   out-bus 0
   time-bus 20
   feedback-bus 21
   mix-bus 22]
  (let [input-sig (o/in in-bus)
        delay-time (o/in:kr time-bus)
        feedback (o/in:kr feedback-bus)
        mix (o/in:kr mix-bus)
        
        delayed (o/comb-c input-sig 2 delay-time feedback)
        mixed (+ (* (- 1 mix) input-sig) (* mix delayed))]
    (cap-out out-bus mixed)))

(o/defsynth reverb-device
  [in-bus 0
   out-bus 0
   room-bus 20
   damp-bus 21
   mix-bus 22]
  (let [input-sig (o/in in-bus)
        room-size (o/in:kr room-bus)
        damping (o/in:kr damp-bus)
        mix (o/in:kr mix-bus)
        
        reverbed (o/free-verb input-sig room-size damping)
        mixed (+ (* (- 1 mix) input-sig) (* mix reverbed))]
    (cap-out out-bus mixed)))

(o/defsynth output-device
  [in-bus 0
   level-bus 20
   pan-bus 21]
  (let [input-sig (o/in in-bus)
        level (o/in:kr level-bus)
        pan (o/in:kr pan-bus)
        
        output-sig (* input-sig level)]
    (cap-out 0 (o/pan2 output-sig pan))))

;; Audio router for connecting devices
(o/defsynth audio-router [in-bus 0 out-bus 0]
  (cap-out out-bus (o/in in-bus)))

;; =============================================================================
;; DEVICE MANAGEMENT
;; =============================================================================

(defn allocate-device-buses!
  "Allocate control and audio buses for a device"
  [device-id device-type]
  (let [control-buses (vec (repeatedly 10 o/control-bus))
        audio-buses (case device-type
                      :audio-osc [(o/audio-bus)]
                      :lfo [(o/control-bus)]
                      :filter [(o/audio-bus) (o/audio-bus)]
                      :mixer [(o/audio-bus) (o/audio-bus) (o/audio-bus)]
                      :delay [(o/audio-bus) (o/audio-bus)]
                      :reverb [(o/audio-bus) (o/audio-bus)]
                      :output [(o/audio-bus)]
                      [])]
    (swap! device-buses assoc device-id {:control control-buses :audio audio-buses})
    {:control control-buses :audio audio-buses}))

(defn get-device-bus
  "Get a specific bus for a device - FIXED VERSION"
  [device-id bus-type index]
  (let [buses (get-in @device-buses [device-id bus-type])]
    (when (and buses (< index (count buses)))
      (nth buses index))))

(defn start-device!
  "Start a device synth with allocated buses - HAS ISSUES WITH BUS PARAMETER PASSING"
  [device-id device-type]
  (let [buses (allocate-device-buses! device-id device-type)
        control-buses (:control buses)
        audio-buses (:audio buses)]
    
    ;; Initialize control buses with default values
    (doseq [[i default-val] (map-indexed vector [0.5 440 0 0 0 0.5 1000 0.3 1 0.5])]
      (o/control-bus-set! (nth control-buses i) default-val))
    
    (let [synth-instance
          ;; NOTE: This bus parameter passing may be incorrect
          (case device-type
            :audio-osc (audio-oscillator-device 
                        :out-bus (.id (first audio-buses))
                        :amp-bus (.id (nth control-buses 0))
                        :freq-bus (.id (nth control-buses 1))
                        :wave-bus (.id (nth control-buses 2))
                        :detune-bus (.id (nth control-buses 3))
                        :osc2-offset-bus (.id (nth control-buses 4)))
            
            :lfo (lfo-device
                  :out-bus (.id (first audio-buses))
                  :freq-bus (.id (nth control-buses 0))
                  :amp-bus (.id (nth control-buses 1))
                  :wave-bus (.id (nth control-buses 2))
                  :offset-bus (.id (nth control-buses 3)))
            
            :filter (filter-device
                     :in-bus (.id (first audio-buses))
                     :out-bus (.id (second audio-buses))
                     :type-bus (.id (nth control-buses 0))
                     :cutoff-bus (.id (nth control-buses 1))
                     :res-bus (.id (nth control-buses 2))
                     :drive-bus (.id (nth control-buses 3)))
            
            :mixer (mixer-device
                    :in-bus-1 (.id (first audio-buses))
                    :in-bus-2 (.id (second audio-buses))
                    :out-bus (.id (nth audio-buses 2))
                    :level1-bus (.id (nth control-buses 0))
                    :level2-bus (.id (nth control-buses 1))
                    :master-bus (.id (nth control-buses 2)))
            
            :delay (delay-device
                    :in-bus (.id (first audio-buses))
                    :out-bus (.id (second audio-buses))
                    :time-bus (.id (nth control-buses 0))
                    :feedback-bus (.id (nth control-buses 1))
                    :mix-bus (.id (nth control-buses 2)))
            
            :reverb (reverb-device
                     :in-bus (.id (first audio-buses))
                     :out-bus (.id (second audio-buses))
                     :room-bus (.id (nth control-buses 0))
                     :damp-bus (.id (nth control-buses 1))
                     :mix-bus (.id (nth control-buses 2)))
            
            :output (output-device
                     :in-bus (.id (first audio-buses))
                     :level-bus (.id (nth control-buses 0))
                     :pan-bus (.id (nth control-buses 1))))]
      
      (swap! device-synths assoc device-id synth-instance)
      (println (str "Started device " device-id " (" device-type ")"))
      synth-instance)))

(defn stop-device!
  "Stop a device synth and free its buses"
  [device-id]
  (when-let [synth-instance (get @device-synths device-id)]
    (try
      (when-not (synth-destroyed? synth-instance)
        (o/kill synth-instance))
      (catch Throwable e
        (def synth-instance0 synth-instance)
        (println "===> EXCEPTION: stop-device!; synth-instance")
        (clojure.pprint/pprint synth-instance)
        (clojure.pprint/pprint e)))
    (swap! device-synths dissoc device-id)
    (println (str "Stopped device " device-id))))

(defn set-device-param!
  "Set a parameter on a device via its control bus - FIXED VERSION"
  [device-id param-index value]
  (when-let [control-buses (get-in @device-buses [device-id :control])]
    (when (< param-index (count control-buses))
      (let [control-bus (nth control-buses param-index)]
        (o/control-bus-set! control-bus value)
        (println (str "Set " device-id " param " param-index " = " value))))))

;; =============================================================================
;; AUDIO ROUTING SYSTEM - HAS ISSUES
;; =============================================================================

(defn connect-audio!
  "Connect audio output of source device to input of destination device"
  [source-device-id source-output-index dest-device-id dest-input-index]
  (let [source-bus (get-device-bus source-device-id :audio source-output-index)
        dest-bus (get-device-bus dest-device-id :audio dest-input-index)]
    (if (and source-bus dest-bus)
      (let [router (audio-router :in-bus (.id source-bus) :out-bus (.id dest-bus))]
        (swap! connection-matrix assoc [source-device-id source-output-index dest-device-id dest-input-index] router)
        (println (str "Connected " source-device-id "[" source-output-index "] → " 
                      dest-device-id "[" dest-input-index "]")))
      (println (str "==> FAIL: connect-audio! " [source-device-id source-output-index dest-device-id dest-input-index])))))

(defn disconnect-audio!
  "Disconnect an audio routing"
  [source-device-id source-output-index dest-device-id dest-input-index]
  (let [connection-key [source-device-id source-output-index dest-device-id dest-input-index]]
    (when-let [router-synth (get @connection-matrix connection-key)]
      (when-not (synth-destroyed? router-synth)
        (o/kill router-synth))
      (swap! connection-matrix dissoc connection-key)
      (println (str "Disconnected " source-device-id "[" source-output-index "] ↛ " 
                   dest-device-id "[" dest-input-index "]")))))

;; =============================================================================
;; KNOB TO DEVICE MAPPING - WORKING VERSION
;; =============================================================================

(defn map-knob-to-device-param!
  "Map a knob to a specific device parameter with real control bus updates"
  [knob-id device-id param-index & {:keys [min-val max-val curve]
                                    :or {min-val 0 max-val 1 curve :linear}}]
  (when-let [knob (get-knob knob-id)]
    (let [updated-knob (assoc knob :min-val min-val :max-val max-val :curve curve)]
      (swap! knob-registry assoc knob-id updated-knob)))
  
  (let [connection-key [:knob knob-id device-id param-index]]
    (swap! patch-matrix assoc connection-key {:device-id device-id :param-index param-index})
    (println (str "Mapped knob " knob-id " → " device-id " param[" param-index "] " 
                  " (range: " min-val " to " max-val ")"))))

(defn set-knob! 
  "Set knob value and update connected device parameters via control buses - WORKING VERSION"
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

(defn create-audio-oscillator! [device-id]
  (create-device! device-id :audio-osc 
                 [:amplitude :base-freq :waveform :detune :osc2-offset]
                 [:audio-out])
  (start-device! device-id :audio-osc))

(defn create-lfo! [device-id] 
  (create-device! device-id :lfo
                 [:frequency :amplitude :waveform :offset]
                 [:cv-out])
  (start-device! device-id :lfo))

(defn create-filter! [device-id]
  (create-device! device-id :filter
                 [:audio-in :filter-type :cutoff :resonance :drive]
                 [:audio-out])
  (start-device! device-id :filter))

(defn create-mixer! [device-id]
  (create-device! device-id :mixer
                 [:audio-in-1 :audio-in-2 :level-1 :level-2 :master-level]
                 [:audio-out])
  (start-device! device-id :mixer))

(defn create-delay! [device-id]
  (create-device! device-id :delay
                 [:audio-in :delay-time :feedback :mix]
                 [:audio-out])
  (start-device! device-id :delay))

(defn create-reverb! [device-id]
  (create-device! device-id :reverb
                 [:audio-in :room-size :damping :mix]
                 [:audio-out])
  (start-device! device-id :reverb))

(defn create-output! [device-id]
  (create-device! device-id :output
                 [:audio-in :level :pan]
                 [])
  (start-device! device-id :output))

;; =============================================================================
;; WORKING SIMPLE SYNTH (ALTERNATIVE APPROACH)
;; =============================================================================

(o/defsynth simple-modular-synth
  "Simplified modular synth with direct parameter control - THIS VERSION WORKS"
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
    
    (cap-out 0 [final-out final-out])))

(defn start-simple-patch! 
  "Start the simple working synth system"
  []
  (when @synth-node
    (o/kill @synth-node))
  (reset! synth-node (simple-modular-synth))
  (println "Simple patch system started!"))

(defn stop-simple-patch!
  "Stop simple patch playback"
  []
  (when @synth-node
    (o/kill @synth-node)
    (reset! synth-node nil))
  (println "Simple patch system stopped!"))

(defn update-synth-param! 
  "Update a synth parameter directly"
  [param value]
  (when @synth-node
    (o/ctl @synth-node param value)))

;; =============================================================================
;; HIGH-LEVEL EXAMPLES
;; =============================================================================

(defn demo-modular-patch!
  "Demonstrate a complete modular patch: Osc → Filter → Output"
  []
  (println "Creating modular patch: Oscillator → Filter → Output")
  
  (create-audio-oscillator! :osc1)
  (create-filter! :filter1) 
  (create-output! :main-out)
  
  (connect-audio! :osc1 0 :filter1 0)
  (connect-audio! :filter1 1 :main-out 0)
  #_(connect-audio! :osc1 0 :main-out 0)
  
  (create-knob! :osc-freq :min-val 220 :max-val 880)
  (create-knob! :osc-amp :min-val 0 :max-val 1)
  (create-knob! :filter-cutoff :min-val 200 :max-val 4000 :curve :exponential)
  (create-knob! :filter-res :min-val 0 :max-val 0.9)
  (create-knob! :output-level :min-val 0 :max-val 1)
  
  (map-knob-to-device-param! :osc-freq :osc1 1 :min-val 220 :max-val 880)
  (map-knob-to-device-param! :osc-amp :osc1 0 :min-val 0 :max-val 1)
  (map-knob-to-device-param! :filter-cutoff :filter1 1 :min-val 200 :max-val 4000)
  (map-knob-to-device-param! :filter-res :filter1 2 :min-val 0 :max-val 0.9)
  (map-knob-to-device-param! :output-level :main-out 0 :min-val 0 :max-val 1)
  
  (set-knob! :osc-freq 0.5)
  (set-knob! :osc-amp 0.7)
  (set-knob! :filter-cutoff 0.6)
  (set-knob! :filter-res 0.3)
  (set-knob! :output-level 0.6)
  
  (println "\nModular patch ready! Try:")
  (println "(set-knob! :osc-freq 0.8)")
  (println "(set-knob! :filter-cutoff 0.2)")
  (println "(set-knob! :filter-res 0.7)")
  #_(println "\nNOTE: This modular routing has issues. Try demo-simple-patch! instead."))

(defn demo-simple-patch!
  "Demonstrate the working simple synth system"
  []
  (println "Creating simple working synth...")
  
  ;; Create knobs that directly control the simple synth
  (create-knob! :simple-osc-amp :min-val 0 :max-val 1)
  (create-knob! :simple-osc-freq :min-val 0.25 :max-val 4 :curve :exponential)  
  (create-knob! :simple-osc-wave :min-val 0 :max-val 3)
  (create-knob! :simple-lfo-rate :min-val 0.1 :max-val 20 :curve :exponential)
  (create-knob! :simple-lfo-depth :min-val 0 :max-val 1)
  (create-knob! :simple-lfo-target :min-val 0 :max-val 2)
  (create-knob! :simple-filter-cutoff :min-val 0.1 :max-val 1 :curve :exponential)
  (create-knob! :simple-filter-res :min-val 0 :max-val 0.9)
  (create-knob! :simple-delay-mix :min-val 0 :max-val 1)
  (create-knob! :simple-reverb-mix :min-val 0 :max-val 1)
  
  ;; Set initial values
  (set-knob! :simple-osc-amp 0.7)
  (set-knob! :simple-osc-freq 0.5)
  (set-knob! :simple-osc-wave 0)
  (set-knob! :simple-lfo-rate 0.3)
  (set-knob! :simple-lfo-depth 0.0)
  (set-knob! :simple-lfo-target 0)
  (set-knob! :simple-filter-cutoff 0.6)
  (set-knob! :simple-filter-res 0.3)
  (set-knob! :simple-delay-mix 0)
  (set-knob! :simple-reverb-mix 0.2)
  
  ;; Start the simple synth
  (start-simple-patch!)
  
  (println "\nSimple synth ready! Try these commands:")
  (println "(set-knob! :simple-osc-freq 0.8)    ; Higher pitch")
  (println "(set-knob! :simple-lfo-depth 0.6)   ; Add tremolo")
  (println "(set-knob! :simple-lfo-target 1)    ; LFO → frequency (vibrato)")
  (println "(set-knob! :simple-filter-cutoff 0.2) ; Dark filter")
  (println "(set-knob! :simple-delay-mix 0.4)   ; Add delay")
  (println "(stop-simple-patch!)                ; Stop audio"))

;; Enhanced knob system for simple synth
(defn set-knob-simple! 
  "Set knob value and directly update simple synth parameters"
  [knob-id value]
  (when-let [knob (get @knob-registry knob-id)]
    (let [clamped-value (max 0.0 (min 1.0 value))
          updated-knob (assoc knob :value clamped-value)
          scaled-value (scale-knob-value clamped-value 
                                        (:min-val knob) 
                                        (:max-val knob) 
                                        (:curve knob))]
      (swap! knob-registry assoc knob-id updated-knob)
      
      ;; Direct synth parameter update mapping for simple synth
      (let [param-map {:simple-osc-amp :osc-amp
                       :simple-osc-freq :osc-freq  
                       :simple-osc-wave :osc-wave
                       :simple-lfo-rate :lfo-rate
                       :simple-lfo-depth :lfo-depth
                       :simple-lfo-target :lfo-target
                       :simple-filter-cutoff :filter-cutoff
                       :simple-filter-res :filter-res
                       :simple-delay-mix :delay-mix
                       :simple-reverb-mix :reverb-mix}
            synth-param (get param-map knob-id)]
        (when synth-param
          (update-synth-param! synth-param scaled-value)))
      
      scaled-value)))

;; Override set-knob! to work with simple synth when appropriate
(defn set-knob! 
  "Set knob value - works with both modular and simple synth systems"
  [knob-id value]
  (if (and @synth-node (.contains (name knob-id) "simple"))
    (set-knob-simple! knob-id value)
    ;; Original modular version
    (when-let [knob (get @knob-registry knob-id)]
      (let [clamped-value (max 0.0 (min 1.0 value))
            updated-knob (assoc knob :value clamped-value)
            scaled-value (scale-knob-value clamped-value 
                                          (:min-val knob) 
                                          (:max-val knob) 
                                          (:curve knob))]
        (swap! knob-registry assoc knob-id updated-knob)
        
        ;; Update all connected device parameters (modular version)
        (doseq [[connection-key connection-info] @patch-matrix]
          (when (and (= (first connection-key) :knob)
                     (= (second connection-key) knob-id))
            (let [{:keys [device-id param-index]} connection-info]
              (set-device-param! device-id param-index scaled-value))))
        
        scaled-value))))

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
    (println (str id " (" (:type device) ")")))
  
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
    (try
      (stop-device! device-id)
      (catch Throwable e
          (println "==> EXCEPTION: stop-all-devices!; device-synths")
          (clojure.pprint/pprint device-id)
          (clojure.pprint/pprint e))))
  (doseq [[_ router-synth] @connection-matrix]
    (try
      (when-not (synth-destroyed? router-synth)
        (o/kill router-synth))
      (catch Throwable e
          (println "==> EXCEPTION: stop-all-devices!; router-synth")
          (clojure.pprint/pprint router-synth)
          (clojure.pprint/pprint e))))
  (reset! device-registry {})
  (reset! device-synths {})
  (reset! device-buses {})
  (reset! connection-matrix {})
  (reset! knob-registry {})
  (reset! patch-matrix {})
  (println "All devices stopped and patch cleared!"))

(defn reset-patch-matrix!
  "Clear all devices, knobs, and connections"
  []
  (stop-all-devices!)
  (stop-simple-patch!)
  #_(reset! bus-registry {})
  (println "Patch matrix completely reset!"))

;; =============================================================================
;; MANUAL DEVICE CREATION HELPERS (FOR DEBUGGING)
;; =============================================================================

(defn create-working-oscillator!
  "Create a working oscillator manually with correct bus setup"
  [device-id]
  (let [amp-bus (o/control-bus)
        freq-bus (o/control-bus)
        wave-bus (o/control-bus)
        detune-bus (o/control-bus)
        offset-bus (o/control-bus)
        out-bus (o/audio-bus)]
    
    ;; Set initial values
    (o/control-bus-set! amp-bus 0.7)
    (o/control-bus-set! freq-bus 440)
    (o/control-bus-set! wave-bus 0)
    (o/control-bus-set! detune-bus 0)
    (o/control-bus-set! offset-bus 0)
    
    ;; Create the oscillator synth
    (let [osc (audio-oscillator-device 
              :out-bus (.id out-bus)
              :amp-bus (.id amp-bus)
              :freq-bus (.id freq-bus)
              :wave-bus (.id wave-bus)
              :detune-bus (.id detune-bus)
              :osc2-offset-bus (.id offset-bus))]
      
      ;; Store buses for later parameter control
      (swap! device-buses assoc device-id {:control [amp-bus freq-bus wave-bus detune-bus offset-bus]
                                          :audio [out-bus]})
      (swap! device-synths assoc device-id osc)
      
      (println (str "Created working oscillator " device-id))
      {:synth osc :buses {:control [amp-bus freq-bus wave-bus detune-bus offset-bus] :audio [out-bus]}})))

(defn test-working-oscillator!
  "Test a manually created working oscillator"
  []
  (println "Creating working test oscillator...")
  (let [result (create-working-oscillator! :test-osc)]
    
    ;; Monitor it directly to speakers
    (let [out-bus (first (get-in result [:buses :audio]))]
      (o/defsynth test-monitor [in-bus 0]
        (cap-out 0 (* 0.3 (o/in in-bus))))
      
      (test-monitor :in-bus (.id out-bus))
      (println "Test oscillator should be audible!")
      
      ;; Test parameter control
      (Thread/sleep 1000)
      (println "Changing frequency...")
      (set-device-param! :test-osc 1 660)  ; Change frequency
      
      (Thread/sleep 1000)
      (println "Changing waveform...")
      (set-device-param! :test-osc 2 1)    ; Change to square wave
      
      (Thread/sleep 1000)
      (println "Test complete - stopping...")
      (o/stop))))

;; =============================================================================
;; USAGE DOCUMENTATION
;; =============================================================================

(comment
  ;; =============================================================================
  ;; USAGE EXAMPLES & WORKFLOWS
  ;; =============================================================================
  
  ;; WORKING APPROACH: Use the simple synth system
  (demo-simple-patch!)                  ; This works reliably
  (set-knob! :simple-osc-freq 0.8)
  (set-knob! :simple-lfo-depth 0.6)
  (set-knob! :simple-filter-cutoff 0.3)
  (stop-simple-patch!)
  
  ;; PROBLEMATIC APPROACH: Modular routing (has issues)
  (demo-modular-patch!) ; Creates devices but audio chain doesn't work

  (let [out-bus (get-device-bus :main-out :audio 0)]
    (o/defsynth monitor-output [in-bus 0]
      (cap-out 0 (* 0.5 (o/in in-bus))))
    (monitor-output :in-bus (.id out-bus)))

  (monitor-device! :filter1 1)

  (show-patch-status)
  (clojure.pprint/pprint @device-buses)
  
  (stop-all-devices!)

  
  (set-knob! :osc-freq 0.8)
  (set-knob! :osc-freq 0.2)
  (set-knob! :osc-freq 0.8)
  (set-device-param! :filter1 1 200)
  (set-device-param! :filter1 1 2000)  
  (set-device-param! :filter1 3 1.0)    ; max drive
  (set-device-param! :filter1 3 0.1)

  (set-device-param! :filter1 2 300)
  (set-device-param! :filter1 3 500)
  (set-device-param! :filter1 4 0.1)  
  (set-device-param! :filter1 5 1.0)

  (set-knob! :filter-cutoff 0.0)
  (set-knob! :filter-cutoff 1.0)
  (set-knob! :filter-res 0.7)

  (set-device-control-bus! :filter1 0 300)
  (set-device-control-bus! :filter1 0 2000)
  
  (set-device-control-bus! :filter1 1 500) ; cutoff
  (set-device-control-bus! :filter1 2 500) ; resonance
  (set-device-control-bus! :filter1 3 1.0) ; drive
  (set-device-control-bus! :filter1 4 500) ; cutoff
  (set-device-control-bus! :filter1 5 0.7) ; resonance
  (set-device-control-bus! :filter1 5 1.0) ; drive

  
  (show-patch-status)
  (clojure.pprint/pprint @device-buses)
  
  (stop-all-devices!)

  (o/defsynth passthrough [in-bus 0]
    (let [sig (o/in in-bus)]
      (o/out 0 sig)))

  (let [output-pan-bus (o/control-bus)
        output-level-bus (o/control-bus)
        output-in-bus (o/audio-bus)

        osc1-out-bus (o/audio-bus)
        osc1-amp-bus (o/control-bus)
        osc1-freq-bus (o/control-bus)
        osc1-wave-bus (o/control-bus)
        osc1-detune-bus (o/control-bus)
        osc1-osc2-offset-bus (o/control-bus)
        osc1-portamento (o/control-bus)
        _ (do (o/control-bus-set! osc1-amp-bus 1.0)
              (o/control-bus-set! osc1-freq-bus 440)
              (o/control-bus-set! osc1-wave-bus 0.0)
              (o/control-bus-set! osc1-detune-bus 0.0)
              (o/control-bus-set! osc1-osc2-offset-bus 0.0)
              (o/control-bus-set! osc1-portamento 0.1))
        osc1 (audio-oscillator-device 
              :out-bus (.id osc1-out-bus)
              :amp-bus (.id osc1-amp-bus)
              :freq-bus (.id osc1-freq-bus)
              :wave-bus (.id osc1-wave-bus)
              :detune-bus (.id osc1-detune-bus)
              :osc2-offset-bus (.id osc1-osc2-offset-bus))
        _ (clojure.pprint/pprint osc1)
        _ (clojure.pprint/pprint (type osc1))
        _ (do (o/control-bus-set! output-level-bus 1.0)
              (o/control-bus-set! output-pan-bus 0.5))
        ar0 (audio-router :in-bus (.id osc1-out-bus)
                          :out-bus (.id output-in-bus))
        output (output-device :in-bus (.id output-in-bus)
                              :level-bus (.id output-level-bus)
                              :pan-bus (.id output-pan-bus))]
    
    output)

  (o/stop)


  
  
  ;; DEBUGGING: Test individual components
  (test-working-oscillator!) ; Creates a working oscillator for testing
  
  ;; STATUS CHECKING
  (show-patch-status)
  @device-synths
  (clojure.pprint/pprint @device-buses)
  @connection-matrix
  (clojure.pprint/pprint @device-buses)
  
  ;; CLEANUP
  (reset-patch-matrix!)

  (o/stop)
    
  (comment))
