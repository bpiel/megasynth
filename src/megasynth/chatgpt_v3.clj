(ns megasynth.chatgpt-v3
  (:require [overtone.live :as o]))

;; ----------------------------
;; Core Data Structures
;; ----------------------------

(defonce knobs (atom {}))
(defonce synth-defs (atom {}))
(defonce bus-registry (atom {}))
(defonce voices (atom []))

;; ----------------------------
;; Utility Functions
;; ----------------------------

(defn scale-knob [val [min max]]
  (+ min (* val (- max min))))

(defn get-knob-bus [k]
  (let [b (get @bus-registry k)]
    (when-not b
      (throw (ex-info "Missing knob bus" {:knob k})))
    b))

(defn set-knob! [k val]
  (let [{:keys [range]} (get @knobs k)
        scaled (if range (scale-knob val range) val)
        bus (get-knob-bus k)]
    (swap! knobs assoc-in [k :value] val)
    (o/control-bus-set! bus scaled)
    scaled))

(defn resolve-param [param bus-map knob-map]
  (cond
    (keyword? param)
    (or (get bus-map param)
        (get knob-map param)
        (throw (ex-info "Cannot resolve param" {:param param})))

    (number? param) param
    :else (throw (ex-info "Unknown param type" {:param param}))))

;; ----------------------------
;; Synth Definitions
;; ----------------------------

(defmulti build-module (fn [type _name _params _resolved] type))

(defmethod build-module :oscillator [_ name params resolved]
  (let [synth-name (symbol (str name "-synth"))]
    (eval
     `(o/defsynth ~synth-name [~'out-bus 0 ~'freq 440 ~'amp 0.5]
        (o/out ~'out-bus (* ~'amp (o/saw ~'freq)))))
    synth-name))

(defmethod build-module :filter [_ name params resolved]
  (let [synth-name (symbol (str name "-synth"))]
    (eval
     `(o/defsynth ~synth-name [~'in-bus 0 ~'out-bus 0 ~'cutoff 440 ~'res 0.5]
        (let [sig (o/in ~'in-bus)]
          (o/out ~'out-bus (o/rlpf sig ~'cutoff ~'res)))))
    synth-name))

(defmethod build-module :output [_ name params resolved]
  (let [synth-name (symbol (str name "-synth"))]
    (eval
     `(o/defsynth ~synth-name [~'in-bus 0 ~'level 1.0 ~'pan 0.5]
        (let [sig (o/in ~'in-bus)]
          (o/out 0 (o/pan2 (* ~'level sig) ~'pan)))))
    synth-name))

;; ----------------------------
;; Voice Instantiation
;; ----------------------------

(defn topological-sort [modules]
  ;; Simplified topological sort
  ;; This assumes each :params only refers to other module names (no cycles)
  (let [deps (fn [mod] (->> (:params (second mod)) vals (filter keyword?)))
        all (keys modules)]
    (loop [remaining all
           sorted []]
      (if (empty? remaining)
        sorted
        (let [next (some #(when (every? (set sorted) (deps [% (modules %)])) %) remaining)]
          (if-not next (throw (ex-info "Cyclic or unresolved dependency" {})))
          (recur (remove #{next} remaining) (conj sorted next)))))))

(defn instantiate-voice! [patch]
  (let [modules (:modules patch)
        knob-defs (:knobs patch)
        knob-buses (into {} (for [[k v] knob-defs]
                              (let [b (o/control-bus)]
                                (swap! bus-registry assoc k b)
                                (o/control-bus-set! b (scale-knob (:value v) (:range v)))
                                [k b])))
        bus-map (atom {})
        instance (atom {})]

    (doseq [k (topological-sort modules)]
      (let [{:keys [type params]} (get modules k)
            resolved (into {} (for [[pk pv] params]
                                [pk (resolve-param pv @bus-map knob-buses)]))
            out-bus (o/audio-bus)
            in-bus (get resolved :in)
            synth-name (build-module type k params resolved)
            args (cond-> [out-bus]
                   (:in resolved) (conj in-bus)
                   (:freq resolved) (conj (:freq resolved))
                   (:amp resolved) (conj (:amp resolved))
                   (:cutoff resolved) (conj (:cutoff resolved))
                   (:res resolved) (conj (:res resolved))
                   (:level resolved) (conj (:level resolved))
                   (:pan resolved) (conj (:pan resolved)))
            node (apply synth-name args)]
        (swap! bus-map assoc k out-bus)
        (swap! instance assoc k node)))
    (swap! voices conj @instance)))

;; ----------------------------
;; Main Entry
;; ----------------------------

(defn compile-patch! [patch voice-count]
  (reset! voices [])
  (reset! bus-registry {})
  (reset! knobs (:knobs patch))
  (dotimes [_ voice-count]
    (instantiate-voice! patch)))

;; ----------------------------
;; Example Patch
;; ----------------------------

(def example-patch
  {:modules
   {:osc1 {:type :oscillator :params {:freq :pitch :amp :amp}}
    :filter1 {:type :filter :params {:in :osc1 :cutoff :cutoff :res :res}}
    :out {:type :output :params {:in :filter1 :level :level :pan :pan}}}
   :knobs
   {:amp {:value 0.5 :range [0 1]}
    :cutoff {:value 0.5 :range [200 4000]}
    :res {:value 0.2 :range [0 0.9]}
    :pitch {:value 0.5 :range [100 880]}
    :level {:value 1.0 :range [0 1]}
    :pan {:value 0.5 :range [0 1]}}})

(comment
  (compile-patch! example-patch 6)
  (set-knob! :cutoff 0.1)
  (set-knob! :res 0.7)
  (set-knob! :amp 0.8))
