(ns megasynth.chatgpt-v2
  (:use [overtone.live])
  (:require [clojure.set :as set]))

;; --- Knob Representation ---

(defn knob-default [knob]
  (let [{:keys [value range]} knob]
    (if range
      (let [[min max] range]
        (+ min (* value (- max min))))
      value)))

;; --- Signal Resolution ---

(defn resolve-input [input env]
  (let [sig-sym (env input)]
    (when (nil? sig-sym)
      (throw (ex-info (str "Unresolved signal: " input) {:input input :env env})))
    sig-sym))

(defn sc-expr [op & args]
  (cons op args))

;; --- Device Generators ---

(defmulti generate-device (fn [type _name _device _env] type))

(defmethod generate-device :lfo [_ name {:keys [inputs]} env]
  (let [freq (resolve-input (get inputs :freq) env)
        amp  (resolve-input (get inputs :amp) env)
        sig-sym (symbol (str name "-out"))]
    {:bindings [[sig-sym (sc-expr '* amp (sc-expr 'sin-osc:kr freq))]]
     :output sig-sym}))

(defmethod generate-device :mixer [_ name {:keys [inputs]} env]
  (let [a-sym (resolve-input (get inputs :a) env)
        b-sym (resolve-input (get inputs :b) env)
        out-sym (symbol (str name "-out"))]
    {:bindings [[out-sym (sc-expr '+ a-sym b-sym)]]
     :output out-sym}))

(defmethod generate-device :oscillator [_ name {:keys [inputs]} env]
  (let [amp-sym (resolve-input (get inputs :amp) env)
        out-sym (symbol (str name "-out"))]
    {:bindings [[out-sym (sc-expr '* amp-sym (sc-expr 'saw 440))]]
     :output out-sym}))

(defmethod generate-device :output [_ _name {:keys [inputs]} env]
  (let [sig-sym (resolve-input (get inputs :in) env)]
    {:out-call (sc-expr 'out 0 sig-sym)}))

;; --- Synth Compilation ---

(defn compile-patch->defsynth [patch synth-name]
  (let [knobs (:knobs patch)
        devices (:devices patch)
        knob-params (mapv (comp symbol name) (keys knobs))
        knob-defaults (mapv (fn [k] (knob-default (knobs k))) (keys knobs))
        env-atom (atom {})
        bind-forms (atom [])
        out-form (atom nil)]

    ;; register knob signals
    (doseq [[k _] knobs]
      (swap! env-atom assoc k (symbol (name k))))

    ;; compile each device
    (doseq [[dev-name dev] devices]
      (let [{:keys [bindings output out-call]} (generate-device (:type dev) dev-name dev @env-atom)]
        (when bindings
          (swap! bind-forms into bindings))
        (when output
          (swap! env-atom assoc dev-name output))
        (when out-call
          (reset! out-form out-call))))

    ;; define the synth
    `(defsynth ~synth-name [~@(interleave knob-params knob-defaults)]
       (let [~@(apply concat @bind-forms)]
         ~@[(or @out-form '(out 0 0.0))]))))

;; --- Example Patch ---

(def patch
  {:knobs {:k0 {:value 0.5 :range [0.0 1.0]}
           :k1 {:value 0.3 :range [0.1 10.0]}
           :k2 {:value 0.4 :range [0.0 1.0]}}

   :devices {:lfo0 {:type :lfo :inputs {:freq :k1 :amp :k2}}
             :mix0 {:type :mixer :inputs {:a :lfo0 :b :k0}}
             :osc0 {:type :oscillator :inputs {:amp :mix0}}
             :out  {:type :output :inputs {:in :osc0}}}})

(comment
  ;; Compile and eval the patch
  (eval (compile-patch->defsynth patch 'modular-synth))

  ;; Boot the server and play
;;  (boot-external-server)

  ;; Start a synth instance
  (def s (modular-synth :k0 0.7 :k1 1.0 :k2 0.6))

  ;; Live tweak knobs
  (ctl s :k0 0.3)
  (ctl s :k1 4.0)
  (ctl s :k2 1.0)

  ;; Free the synth
  (kill s))
