(ns megasynth.common
  (:require [overtone.live :as o]))

(defonce synths-ifaces& (atom {}))


(defmacro mk-synth [args body]
  `(o/synth ~(into ['freq 440
                    'gate 1]
                   (mapcat (fn [[a [_ v]]] [a v])
                           (partition 2 args)))
     ~body))

(defn ->arg-states [args]
  (apply hash-map (mapcat (fn [[a [_ v]]] [(keyword a) v])
                          (partition 2 args))))

(defn ->mono [syn]
  (let [mono& (atom nil)]
    (fn [& args]
      (when-let [s @mono&]
        (when-not (-> s :status deref (= :destroyed))
          (o/kill s))) 
      (reset! mono&
              (apply syn args)))))

(defmacro mk-synth-iface [args body & [{:keys [mono?]}]]
  (let [arg-states (->arg-states args)
        args' (vec (partition 2 args))
        syn (if mono?
              `(->mono (mk-synth ~args ~body))
              `(mk-synth ~args ~body))]
    `{:args '~args'
      :arg-init ~arg-states
      :arg-states ~arg-states
      :synth ~syn}))

(defn ugen-semitone-ratio [semi]
  (o/pow 2.0 (o/mul-add semi (/ 1.0 12.0) 0)))

(defn ugen-semitone-ratio-floor [semi]
  (o/pow 2.0 (o/mul-add (o/floor semi) (/ 1.0 12.0) 0)))

(defn ugen-semitone-ratio-1 [semi]
  (o/mul-add (o/pow 2.0 (o/mul-add semi (/ 1.0 12.0) 0.0))
             1.0 -1.0))
