(ns megasynth.bill-v1
  (:require [megasynth.common :as com]
            megasynth.synths.da-funk
            [overtone.live :as o]
            [overtone.midi :as midi])
  (:import [javax.sound.midi MidiDevice MidiDevice$Info MidiSystem
            Receiver Sequencer ShortMessage Synthesizer SysexMessage Transmitter]))

(defn loading-tones [done?]
  (let [[f r s] (if done?
                [660 1.2 500]
                [440 0.1 300])
        sn0 (o/synth [freq 440
                      amp 0.5
                      attack 0.01
                      decay 0.0
                      sustain 1.0
                      release 0.3
                      gate 1.0]
              (let [env (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
                    sig (* amp env (o/sin-osc freq))]
                (o/out 0 (o/pan2 sig))))
        vc (sn0 :freq f :release r)]
    (Thread/sleep s)
    (o/ctl vc :gate 0.0)))

;; START LOADING 
(loading-tones false)

;; === START - STATE ======

(defonce midi-msg-fn& (atom identity))

(defonce current-midi& (atom nil))

(defonce current-synth& (atom nil)) ;; keyword

(def notes& (atom {}))

;; === END - STATE ======


(defn map->vec [m]
  (vec (mapcat identity m)))

(defn midi->hz [note]
  (* 440 (Math/pow 2 (/ (- note 69) 12.0))))

(defn set-synth! [s-kw]
  (reset! current-synth& s-kw)
  (swap! com/synths-ifaces& assoc-in
         [s-kw :arg-states]
         (-> @com/synths-ifaces& s-kw :arg-init)))

(defn scale-knob-value
  "Convert 0.0-1.0 knob value to actual parameter range"
  [knob-value min-val max-val curve]
  (case curve
    :linear (+ min-val (* knob-value (- max-val min-val)))
    :exponential (+ min-val (* (Math/pow knob-value 2) (- max-val min-val)))
    :logarithmic (+ min-val (* (Math/sqrt knob-value) (- max-val min-val)))
    (+ min-val (* knob-value (- max-val min-val)))))


(defn destroyed? [node]
  (def node0 node)
  (-> node :status deref (= :destroyed)))

(defn note-on! [note-id]
  (try
    (let [freq (midi->hz note-id)
          syn-kw @current-synth&
          syn-iface (@com/synths-ifaces& syn-kw)
          {syn :synth :keys [arg-states]} syn-iface
;;          _ (clojure.pprint/pprint (apply vector syn :freq freq (map->vec arg-states)))
          node1 (apply syn :freq freq (map->vec arg-states))]
      (swap! notes& assoc note-id node1)
      node1)
    (catch Throwable e
      (clojure.pprint/pprint e))))

#_(note-on! 60)

(defn note-off! [note-id]
  (when-let [node0 (@notes& note-id)]
    (try
      (when-not (-> node0 :status deref (= :destroyed))
        (o/ctl node0 :gate 0.0))
      (catch Throwable e
        (def node00 node0)
        (def node00-state (-> node0 :status deref))
        (clojure.pprint/pprint e)
        (clojure.pprint/pprint node0))))
  true)

(defn change-synth-arg! [idx val]
  (let [current-synth @current-synth&
        {:keys [synth args]} (@com/synths-ifaces& current-synth)
        [arg-sym [lo _ hi]] (nth args idx)
        arg-kw (-> arg-sym name keyword)
        new-val (scale-knob-value (/ val 127.0)
                                  lo
                                  hi
                                  :linear)
        notes @notes&]
    (clojure.pprint/pprint [idx val arg-sym new-val lo hi])        
    #_(clojure.pprint/pprint [synth arg-kw new-val])
    (try
      (swap! com/synths-ifaces& assoc-in
             [current-synth :arg-states arg-kw] new-val)
      (catch Throwable e
        (clojure.pprint/pprint e)))
    (doseq [[_ n] notes]
      (try
        (when-not (destroyed? n)
          (o/ctl n arg-kw new-val))
        (catch Throwable e
          (clojure.pprint/pprint [e synth arg-kw new-val]))))
    true))

(defn control-change! [{:keys [command note status data1 data2 velocity] :as msg}]
  (try
    (case data1
      14 (set-synth! :da-funk)
      15 (set-synth! :final-countdown)
      16 (set-synth! :volca-keys0)
      17 (set-synth! :atw-lead-1)
      18 (set-synth! :jump)
      19 (set-synth! :poly)                  
      (change-synth-arg! (- data1 102) data2))
    (catch Throwable e
      (clojure.pprint/pprint e))))

(defn short-print-midi-msg [{:keys [command note status data1 data2 velocity] :as msg}]
  (format "cmd %s, status %s, note %d, vel %d, data [%d %d]\n"
          command status note velocity data1 data2))

(defn midi-handler [{:keys [command note] :as msg}]
  #_(clojure.pprint/pprint msg)
  (println (short-print-midi-msg msg))
  (case command
    :note-on (note-on! note)
    :note-off (note-off! note)
    :control-change (control-change! msg)
    nil
    #_(clojure.pprint/pprint msg)))

#_(midi-handler {:command :note-on :note 60})
#_(midi-handler {:command :note-off :note 60})

(defn mk-receiver []
  (proxy [Receiver] []
    (close [] nil)
    (send [msg timestamp] (cond (instance? ShortMessage msg )
                                (@midi-msg-fn&
                                 (assoc (midi/midi-msg msg timestamp)
                                        :device :DUMMY))

                                (instance? SysexMessage msg)
                                (@midi-msg-fn&
                                 {:timestamp timestamp
                                  :data (.getData ^SysexMessage msg)
                                  :status (.getStatus ^SysexMessage msg)
                                  :length (.getLength ^SysexMessage msg)
                                  :device :DUMMY})))))

(defn setup-midi-in []
  (let [m-in (midi/midi-in "O")]
    (if-not (nil? m-in)
      (let [_ (reset! current-midi& m-in)
            rcvr (mk-receiver)
            route (midi/midi-route m-in
                                   {:receiver rcvr})]
        true)
      (throw (Exception. "MIDI INPUT NOT FOUND")))))

#_(setup-midi-in)

(defn setup-all []
  (reset! midi-msg-fn& midi-handler)
  (reset! current-synth& :da-funk)
  (setup-midi-in))

;; DONE LOADING ==========
(loading-tones true)
;; =======================

#_(setup-all)

(comment

  (set-synth! da-funk-mono)

  (def fc-lead-501-mono (->mono fc-lead-x))

  (do (def fc-lead-501-mono (->mono fc-lead-x))
      (set-synth! fc-lead-501-mono))

  (do (def fc-lead-501-mono (->mono fc-lead-x2))
      (set-synth! fc-lead-501-mono))

  (do (def fc-lead-501-mono (->mono fc-lead-x3))
      (set-synth! fc-lead-501-mono))

  (do (def fc-lead-501-mono (->mono fc-lead-x4))
      (set-synth! fc-lead-501-mono))

  (play fc-lead-x4 400)

  (comment))

#_(set-synth! fc-lead-501-mono)

#_(set-synth! da-funk-mono0)


#_(set-synth! :volca-keys0)

