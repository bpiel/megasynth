(ns megasynth.claude4-v20
  "🎛️ Megasynth: Graph-Based Polyphonic Modular Synth System"
  (:require [overtone.core :as o]
            [overtone.sc.bus :as bus]
            [clojure.set :as set]))

;; =============================================================================
;; 🔧 Core State Management
;; =============================================================================

(def ^:private system-state
  "Central atom holding the entire system state"
  (atom {:patch-graph nil
         :compiled-state nil
         :voices []
         :knobs {}
         :buses {}
         :synth-defs {}
         :num-voices 8}))

;; =============================================================================
;; 🎛️ Knob Management
;; =============================================================================

(defn- allocate-knob-bus
  "Allocate a control-rate bus for a knob"
  [knob-id initial-value]
  (let [bus (bus/control-bus)]
    (o/control-bus-set! bus initial-value)
    bus))

(defn set-knob!
  "Set a knob value in real-time"
  [knob-id value]
  (when-let [knob-info (get-in @system-state [:knobs knob-id])]
    (let [{:keys [bus range scale]} knob-info
          scaled-value (case scale
                        :exp (Math/pow value (or range 1.0))
                        :lin (* value (or range 1.0))
                        value)]
      (o/control-bus-set! bus scaled-value)
      (swap! system-state assoc-in [:knobs knob-id :current-value] value))))

(defn get-knob
  "Get current knob value"
  [knob-id]
  (get-in @system-state [:knobs knob-id :current-value]))

(defn- setup-knobs!
  "Initialize knobs from patch graph"
  [patch-graph]
  (let [knobs (:knobs patch-graph)]
    (doseq [[knob-id knob-config] knobs]
      (let [initial-value (:initial knob-config 0.5)
            bus (allocate-knob-bus knob-id initial-value)]
        (swap! system-state assoc-in [:knobs knob-id]
               {:bus bus
                :current-value initial-value
                :range (:range knob-config)
                :scale (:scale knob-config :lin)})))))

;; =============================================================================
;; 🔌 Bus Management
;; =============================================================================

(defn- allocate-buses-for-voice
  "Allocate buses for a single voice"
  [voice-id modules]
  (into {}
        (map (fn [[module-id module-config]]
               [module-id {:out-bus (bus/audio-bus)}])
             modules)))

(defn- allocate-all-buses!
  "Allocate buses for all voices"
  [patch-graph]
  (let [modules (:modules patch-graph)
        num-voices (:num-voices @system-state)]
    (swap! system-state assoc :buses
           (into {}
                 (map (fn [voice-id]
                        [voice-id (allocate-buses-for-voice voice-id modules)])
                      (range num-voices))))))

;; =============================================================================
;; 📊 Graph Analysis
;; =============================================================================

(defn- get-dependencies
  "Get dependencies for a module from connections"
  [module-id connections]
  (->> connections
       (filter (fn [[_ _ target-module _]] (= target-module module-id)))
       (map (fn [[source-module _ _ _]] source-module))
       set))

(defn- topological-sort
  "Topologically sort modules based on their dependencies"
  [modules connections]
  (let [deps (into {}
                   (map (fn [[module-id _]]
                          [module-id (get-dependencies module-id connections)])
                        modules))
        
        ;; Kahn's algorithm
        sorted (atom [])
        in-degree (atom (into {}
                              (map (fn [[module-id module-deps]]
                                     [module-id (count module-deps)])
                                   deps)))
        queue (atom (filter #(zero? (get @in-degree %)) (keys modules)))]
    
    (while (seq @queue)
      (let [current (first @queue)]
        (swap! queue rest)
        (swap! sorted conj current)
        
        ;; Update in-degrees of dependent modules
        (doseq [[module-id module-deps] deps]
          (when (contains? module-deps current)
            (swap! in-degree update module-id dec)
            (when (zero? (get @in-degree module-id))
              (swap! queue conj module-id))))))
    
    @sorted))

;; =============================================================================
;; 🎹 Pre-defined Synth Definitions
;; =============================================================================

(o/defsynth megasynth-osc
  [out-bus 0
   in-bus 0
   freq-knob-bus 0
   freq 440
   wave 0]
  (let [freq-val (+ freq (o/in:kr freq-knob-bus))
        saw      (o/saw freq-val)
        pulse    (o/pulse freq-val)
        sin      (o/sin-osc freq-val)
        tri      (o/lf-tri freq-val)
        waveforms [saw pulse sin tri]
        wave-signal (o/select wave waveforms)]
    (o/out out-bus wave-signal)))

(o/defsynth megasynth-filter
  [out-bus 0
   in-bus 0
   cutoff-knob-bus 0
   res-knob-bus 0
   cutoff 1000
   res 0.5
   filter-type 0]
  (let [input-sig (o/in in-bus)
        cutoff-val (+ cutoff (o/in:kr cutoff-knob-bus))
        res-val (+ res (o/in:kr res-knob-bus))
        lpf-sig (o/lpf input-sig cutoff-val)
        hpf-sig (o/hpf input-sig cutoff-val)
        bpf-sig (o/bpf input-sig cutoff-val res-val)
        filters [lpf-sig hpf-sig bpf-sig]
        filtered-signal (o/select filter-type filters)]
    (o/out out-bus filtered-signal)))

(o/defsynth megasynth-amp
  [out-bus 0
   in-bus 0
   amp-knob-bus 0
   amp 0.5]
  (let [input-sig (o/in in-bus)
        amp-val (+ amp (o/in:kr amp-knob-bus))]
    (o/out out-bus (* input-sig amp-val))))

(o/defsynth megasynth-output
  [out-bus 0
   in-bus 0]
  (let [input-sig (o/in in-bus)]
    (o/out 0 input-sig)))

;; Map module types to their synth functions
(def module-synths
  {:osc megasynth-osc
   :filter megasynth-filter  
   :amp megasynth-amp
   :output megasynth-output})

;; Map wave types to numbers for synth params
(def wave-types
  {:saw 0 :square 1 :sine 2 :triangle 3})

;; Map filter types to numbers
(def filter-types  
  {:lpf 0 :hpf 1 :bpf 2})

;; =============================================================================
;; 🎵 Voice Management
;; =============================================================================

(defn- build-synth-args
  "Build arguments for instantiating a module synth"
  [module-id module-config connections voice-buses knobs]
  (let [params (:params module-config {})
        module-type (:type module-config)]
    
    (concat
     ;; Output bus (always first)
     [:out-bus (get-in voice-buses [module-id :out-bus])]
     
     ;; Input bus (if this module has inputs)
     (when-let [input-connection (first (filter #(= (nth % 2) module-id) connections))]
       (let [source-module (first input-connection)]
         [:in-bus (get-in voice-buses [source-module :out-bus])]))
     
     ;; Knob buses and static params based on module type
     (case module-type
       :osc (concat
             (when-let [freq-knob (:freq params)]
               (if (keyword? freq-knob)
                 [:freq-knob-bus (get-in knobs [freq-knob :bus])]
                 [:freq (:freq params 440)]))
             [:wave (get wave-types (:wave params :saw) 0)])
       
       :filter (concat
                (when-let [cutoff-knob (:cutoff params)]
                  (if (keyword? cutoff-knob)
                    [:cutoff-knob-bus (get-in knobs [cutoff-knob :bus])]
                    [:cutoff (:cutoff params 1000)]))
                (when-let [res-knob (:res params)]
                  (if (keyword? res-knob)
                    [:res-knob-bus (get-in knobs [res-knob :bus])]
                    [:res (:res params 0.5)]))
                [:filter-type (get filter-types (:filter-type params :lpf) 0)])
       
       :amp (when-let [amp-knob (:amp params)]
              (if (keyword? amp-knob)
                [:amp-knob-bus (get-in knobs [amp-knob :bus])]
                [:amp (:amp params 0.5)]))
       
       :output []
       
       ;; Default
       []))))

(defn- instantiate-voice-synths
  "Instantiate synths for a single voice in topological order"
  [voice-id sorted-modules patch-graph]
  (let [modules (:modules patch-graph)
        connections (:connections patch-graph)
        knobs (:knobs @system-state)
        voice-buses (get-in @system-state [:buses voice-id])]
    
    (into {}
          (keep (fn [module-id]
                  (let [module-config (get modules module-id)
                        module-type (:type module-config)
                        synth-fn (get module-synths module-type)
                        synth-args (build-synth-args module-id module-config connections voice-buses knobs)]
                    
                    (when synth-fn
                      [module-id (apply synth-fn synth-args)])))
                sorted-modules))))

(defn- create-voice
  "Create a complete voice with all its synths"
  [voice-id patch-graph sorted-modules]
  (instantiate-voice-synths voice-id sorted-modules patch-graph))

;; =============================================================================
;; 🔄 Patch Compilation
;; =============================================================================

(defn- compile-patch-graph
  "Compile a patch graph into a running system"
  [patch-graph]
  (let [modules (:modules patch-graph)
        connections (:connections patch-graph)
        sorted-modules (topological-sort modules connections)]
    
    (println "🔧 Compiling patch graph...")
    (println "📊 Module order:" sorted-modules)
    
    ;; Setup knobs
    (setup-knobs! patch-graph)
    
    ;; Allocate buses
    (allocate-all-buses! patch-graph)
    
    ;; Create voices (synth definitions are pre-compiled)
    (let [num-voices (:num-voices @system-state)
          voices (into {}
                       (map (fn [voice-id]
                              [voice-id (create-voice voice-id patch-graph sorted-modules)])
                            (range num-voices)))]
      
      (swap! system-state assoc :voices voices)
      (swap! system-state assoc :compiled-state
             {:patch-graph patch-graph
              :sorted-modules sorted-modules
              :timestamp (System/currentTimeMillis)})
      
      (println "✅ Patch compiled successfully with" num-voices "voices"))))

;; =============================================================================
;; 🔥 Hot Reloading
;; =============================================================================

(defn- diff-patch-graphs
  "Compare two patch graphs and return what changed"
  [old-graph new-graph]
  (let [old-modules (set (keys (:modules old-graph)))
        new-modules (set (keys (:modules new-graph)))
        old-connections (set (:connections old-graph))
        new-connections (set (:connections new-graph))
        old-knobs (set (keys (:knobs old-graph)))
        new-knobs (set (keys (:knobs new-graph)))]
    
    {:modules-added (set/difference new-modules old-modules)
     :modules-removed (set/difference old-modules new-modules)
     :modules-changed (set/intersection old-modules new-modules) ;; TODO: deeper diff
     :connections-added (set/difference new-connections old-connections)
     :connections-removed (set/difference old-connections new-connections)
     :knobs-added (set/difference new-knobs old-knobs)
     :knobs-removed (set/difference old-knobs new-knobs)}))

(declare stop-all-voices!)

(defn- hot-reload-patch
  "Hot-reload a patch with minimal disruption"
  [new-patch-graph]
  (if-let [old-compiled-state (:compiled-state @system-state)]
    (let [old-patch-graph (:patch-graph old-compiled-state)
          diff (diff-patch-graphs old-patch-graph new-patch-graph)]
      
      (println "🔄 Hot-reloading patch...")
      (println "📊 Changes detected:" diff)
      
      ;; For now, do a full recompile if anything significant changed
      ;; TODO: Implement selective updates
      (if (or (seq (:modules-added diff))
              (seq (:modules-removed diff))
              (seq (:connections-added diff))
              (seq (:connections-removed diff)))
        (do
          (println "🔄 Significant changes detected, doing full recompile...")
          (stop-all-voices!)
          (compile-patch-graph new-patch-graph))
        (do
          (println "✅ No significant changes, preserving current state")
          (swap! system-state assoc-in [:compiled-state :patch-graph] new-patch-graph))))
    
    ;; No previous state, do full compile
    (compile-patch-graph new-patch-graph)))

;; =============================================================================
;; 🎹 Voice Control
;; =============================================================================

(defn- stop-voice!
  "Stop all synths in a voice"
  [voice-id]
  (when-let [voice-synths (get-in @system-state [:voices voice-id])]
    (doseq [[_ synth] voice-synths]
      (o/kill synth)))
  (swap! system-state assoc-in [:voices voice-id] nil))

(defn- stop-all-voices!
  "Stop all voices"
  []
  (doseq [voice-id (keys (:voices @system-state))]
    (stop-voice! voice-id)))

(defn- find-free-voice
  "Find a free voice for note-on"
  []
  (let [voices (:voices @system-state)]
    (first (filter (fn [voice-id]
                     (or (nil? (get voices voice-id))
                         (not (get-in @system-state [:voice-states voice-id :active]))))
                   (keys voices)))))

(defn note-on!
  "Trigger a note on a free voice"
  [freq velocity]
  (when-let [voice-id (find-free-voice)]
    (println "🎵 Note on: voice" voice-id "freq" freq "vel" velocity)
    ;; Set frequency knob if it exists
    (when (contains? (:knobs @system-state) :freq)
      (set-knob! :freq freq))
    ;; Mark voice as active
    (swap! system-state assoc-in [:voice-states voice-id :active] true)
    voice-id))

(defn note-off!
  "Release a voice"
  [voice-id]
  (when voice-id
    (println "🎵 Note off: voice" voice-id)
    ;; Mark voice as inactive
    (swap! system-state assoc-in [:voice-states voice-id :active] false)))

;; =============================================================================
;; 🎛️ Public API
;; =============================================================================

(defn load-patch!
  "Load a new patch graph"
  [patch-graph]
  (if (:compiled-state @system-state)
    (hot-reload-patch patch-graph)
    (compile-patch-graph patch-graph)))

(defn stop-patch!
  "Stop the current patch"
  []
  (stop-all-voices!)
  (swap! system-state assoc :compiled-state nil))

(defn get-system-state
  "Get current system state (for debugging)"
  []
  @system-state)

(defn set-num-voices!
  "Set the number of voices"
  [n]
  (swap! system-state assoc :num-voices n))

;; =============================================================================
;; 🎼 Example Patch Graphs
;; =============================================================================

(def example-patch-basic
  "Basic oscillator -> filter -> amp -> output patch"
  {:modules {:osc1 {:type :osc
                    :params {:freq :freq-knob
                             :wave :saw}}
             :filter1 {:type :filter
                       :params {:cutoff :cutoff-knob
                                :res :res-knob
                                :filter-type :lpf}}
             :amp1 {:type :amp
                    :params {:amp :amp-knob}}
             :out {:type :output}}
   
   :connections [[:osc1 :out :filter1 :in]
                 [:filter1 :out :amp1 :in]
                 [:amp1 :out :out :in]]
   
   :knobs {:freq-knob {:initial 440 :range 2000 :scale :exp}
           :cutoff-knob {:initial 0.5 :range 2000 :scale :exp}
           :res-knob {:initial 0.3 :range 1.0 :scale :lin}
           :amp-knob {:initial 0.7 :range 1.0 :scale :lin}}})

(def example-patch-dual-osc
  "Dual oscillator patch with mixing"
  {:modules {:osc1 {:type :osc
                    :params {:freq :freq-knob
                             :wave :saw}}
             :osc2 {:type :osc
                    :params {:freq :freq2-knob
                             :wave :square}}
             :mix {:type :amp
                   :params {:amp :mix-knob}}
             :filter1 {:type :filter
                       :params {:cutoff :cutoff-knob
                                :res :res-knob}}
             :amp1 {:type :amp
                    :params {:amp :amp-knob}}
             :out {:type :output}}
   
   :connections [[:osc1 :out :mix :in]
                 [:osc2 :out :mix :in]  ;; Note: This is a simplification
                 [:mix :out :filter1 :in]
                 [:filter1 :out :amp1 :in]
                 [:amp1 :out :out :in]]
   
   :knobs {:freq-knob {:initial 440 :range 2000 :scale :exp}
           :freq2-knob {:initial 445 :range 2000 :scale :exp}
           :mix-knob {:initial 0.5 :range 1.0 :scale :lin}
           :cutoff-knob {:initial 0.5 :range 2000 :scale :exp}
           :res-knob {:initial 0.3 :range 1.0 :scale :lin}
           :amp-knob {:initial 0.7 :range 1.0 :scale :lin}}})

;; =============================================================================
;; 🎯 Quick Start Functions
;; =============================================================================

(defn quick-start!
  "Quick start with basic patch"
  []
  (println "🎛️ Starting Megasynth...")
  (load-patch! example-patch-basic)
  (println "🎹 Use (note-on! 440 0.8) and (note-off! voice-id) to play notes")
  (println "🎛️ Use (set-knob! :cutoff-knob 0.3) to adjust parameters"))

(defn demo!
  "Run a quick demo"
  []
  (quick-start!)
  (Thread/sleep 500)
  (let [voice1 (note-on! 440 0.8)]
    (Thread/sleep 1000)
    (set-knob! :cutoff-knob 0.8)
    (Thread/sleep 1000)
    (set-knob! :cutoff-knob 0.2)
    (Thread/sleep 1000)
    (note-off! voice1)
    (println "🎵 Demo complete!")))

;; =============================================================================
;; 🎛️ Usage Examples
;; =============================================================================

(comment

  (demo!)

  (o/stop)
  
  ;; Basic usage
  (quick-start!)
  
  ;; Play some notes
  (def voice1 (note-on! 440 0.8))
  (def voice2 (note-on! 554 0.6))
  (note-off! voice1)
  (note-off! voice2)
  
  ;; Adjust parameters
  (set-knob! :cutoff-knob 0.8)
  (set-knob! :res-knob 0.7)
  (set-knob! :amp-knob 0.5)
  
  ;; Load different patch
  (load-patch! example-patch-dual-osc)
  
  ;; Hot reload (modify and reload the same patch)
  (load-patch! (assoc-in example-patch-basic [:knobs :cutoff-knob :initial] 0.8))
  
  ;; System inspection
  (get-system-state)
  (get-knob :cutoff-knob)
  
  ;; Cleanup
  (stop-patch!)
  )
