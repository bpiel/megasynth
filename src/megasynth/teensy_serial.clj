(ns megasynth.teensy-serial
  (:require
   [clojure.string :as str])
  (:import
   [com.fazecast.jSerialComm SerialPort]
   [java.io InputStream OutputStream IOException]))

;; Teensy protocol:
;;
;; Teensy -> Clojure framed messages:  MSG_START type [raw args] MSG_END
;;   MSG_START = 0x2A '*'   MSG_END = 0x0A newline.
;;   Both markers are printable-ish so frame boundaries and the type char are
;;   legible in the Arduino serial monitor; the args stay raw bytes.
;;
;;   * E <index> <delta>   encoder turn (index 0-3, delta raw 1 or 255 = -1)
;;   * R                   ready (boot, or answer to READY?)
;;   * P                   pong  (answer to PING)
;;
;; Clojure -> Teensy:
;;   Text commands, newline terminated. NOT yet converted to the framed form.
;;
;; Display:
;;   D<TAB>index<TAB>label<TAB>value<TAB>amount
;;
;; Other:
;;   PING
;;   READY?
;;   CLEAR
;;   INV<TAB>index<TAB>0-or-1

(def serial-baud 115200)

;; Frame markers (see protocol note above).
(def msg-start 0x2A)               ; '*'
(def msg-end   0x0A)               ; newline

(def encoder-command (int \E))     ; * E <index> <delta>
(def ready-command   (int \R))     ; * R
(def pong-command    (int \P))     ; * P

;; Number of raw argument bytes that follow each type char.
(def message-arg-counts
  {encoder-command 2
   ready-command   0
   pong-command    0})

(def post-ready-quiet-ms 250)

(defonce !port (atom nil))
(defonce !running? (atom false))
(defonce !reader-thread (atom nil))

(defonce !teensy-ready? (atom false))
(defonce !ignore-encoders-until-ms (atom 0))

(defonce !encoder-handler
  (atom
   (fn [index delta]
     ;; Replace this with your real synth/control dispatch.
     (println :encoder index :delta delta))))

(defonce !line-handler
  (atom
   (fn [msg]
     ;; Non-encoder Teensy messages, as keywords: :ready (boot / READY?), :pong.
     (println :teensy-line msg))))

(declare close-port! write-line!)

;; -----------------------------
;; Time helpers
;; -----------------------------

(defn now-ms []
  (quot (System/nanoTime) 1000000))

(defn ready? []
  @!teensy-ready?)

;; -----------------------------
;; Port discovery
;; -----------------------------

(defn list-ports []
  (mapv (fn [^SerialPort p]
          {:system-port-name (.getSystemPortName p)
           :descriptive-port-name (.getDescriptivePortName p)
           :port-description (.getPortDescription p)})
        (SerialPort/getCommPorts)))

(defn- teensy-ish-port? [^SerialPort p]
  (let [haystack (str (.getSystemPortName p)
                      " "
                      (.getDescriptivePortName p)
                      " "
                      (.getPortDescription p))]
    (boolean
     (re-find #"(?i)ttyACM|ttyUSB|Teensy|USB Serial" haystack))))

(defn find-port
  "Crude helper. On Linux this will usually be ttyACM0/ttyACM1 for Teensy."
  []
  (let [ports (seq (SerialPort/getCommPorts))]
    (or
     (some (fn [^SerialPort p]
             (when (teensy-ish-port? p)
               p))
           ports)
     (first ports))))

;; -----------------------------
;; Opening / closing
;; -----------------------------

(defn open-port!
  ([]
   (open-port! (find-port)))
  ([port-or-name]
   ;; Prevent two readers from fighting over the same serial stream.
   (when @!port
     (close-port!))

   (let [^SerialPort port
         (cond
           (nil? port-or-name)
           (throw (ex-info "No serial port found"
                           {:available-ports (list-ports)}))

           (instance? SerialPort port-or-name)
           port-or-name

           (string? port-or-name)
           (SerialPort/getCommPort port-or-name)

           :else
           (throw (ex-info "Expected SerialPort or port name"
                           {:got port-or-name})))]

     (.setComPortParameters port
                            serial-baud
                            8
                            SerialPort/ONE_STOP_BIT
                            SerialPort/NO_PARITY)

     ;; Important:
     ;; Use blocking reads on a dedicated reader thread.
     ;;
     ;; The earlier semi-blocking + try/catch approach could read MSG_START,
     ;; then time out before the type/args/terminator arrived, abandoning the
     ;; frame.
     ;;
     ;; Timeout 0 = wait indefinitely for bytes.
     (.setComPortTimeouts port
                          SerialPort/TIMEOUT_READ_BLOCKING
                          0
                          0)

     (when-not (.openPort port)
       (throw (ex-info "Could not open serial port"
                       {:port (.getSystemPortName port)
                        :available-ports (list-ports)})))

     (reset! !teensy-ready? false)
     (reset! !ignore-encoders-until-ms 0)
     (reset! !port port)

     port)))

(defn close-port! []
  (reset! !running? false)

  ;; Closing the port unblocks the blocking .read on the reader thread.
  (when-let [^SerialPort port @!port]
    (when (.isOpen port)
      (.closePort port)))

  (when-let [^Thread t @!reader-thread]
    (when (and (not= t (Thread/currentThread))
               (.isAlive t))
      (.join t 1000)))

  (reset! !reader-thread nil)
  (reset! !port nil)
  (reset! !teensy-ready? false)
  (reset! !ignore-encoders-until-ms 0)

  :closed)

;; -----------------------------
;; Byte helpers
;; -----------------------------

(defn ubyte [b]
  (bit-and (long b) 0xFF))

(defn signed-byte [b]
  (let [x (ubyte b)]
    (if (> x 127)
      (- x 256)
      x)))

(defn read-byte!
  "Blocking read. Returns unsigned byte 0-255.
   Throws if the stream/port closes."
  [^InputStream in]
  (let [b (.read in)]
    (when (= b -1)
      (throw (IOException. "Serial input stream closed")))
    (ubyte b)))

;; -----------------------------
;; Event handlers
;; -----------------------------

(defn set-encoder-handler!
  "Install the real synth/control callback.

   Callback receives:
     index: 0-3
     delta: -1 or +1"
  [f]
  (when-not (fn? f)
    (throw (ex-info "Encoder handler must be a function" {:got f})))
  (reset! !encoder-handler f)
  :ok)

(defn set-line-handler!
  "Optional callback for non-encoder Teensy messages, called with a keyword
   (:ready, :pong)."
  [f]
  (when-not (fn? f)
    (throw (ex-info "Line handler must be a function" {:got f})))
  (reset! !line-handler f)
  :ok)

(defn valid-encoder-event? [index delta]
  (and (<= 0 index)
       (< index 4)
       (contains? #{-1 1} delta)))

(defn handle-encoder-event!
  [index delta]
  (@!encoder-handler index delta))

(defn- mark-ready! []
  (reset! !teensy-ready? true)
  (reset! !ignore-encoders-until-ms
          (+ (now-ms) post-ready-quiet-ms)))

(defn- handle-complete-encoder-packet!
  [index delta-byte]
  (let [delta (signed-byte delta-byte)]
    (cond
      ;; Ignore events until the Teensy has announced boot readiness.
      ;; This avoids weird packets during Teensy reset / USB reconnect.
      (not @!teensy-ready?)
      nil

      ;; Tiny settle period immediately after READY.
      (< (now-ms) @!ignore-encoders-until-ms)
      nil

      (valid-encoder-event? index delta)
      (handle-encoder-event! index delta)

      :else
      (println :bad-encoder-event
               {:index index
                :delta delta
                :raw-delta delta-byte}))))

(defn- dispatch-message!
  "Handle one fully-framed Teensy message: a type char plus its raw arg bytes."
  [type args]
  (condp = type
    encoder-command
    (let [[index delta-byte] args]
      (handle-complete-encoder-packet! index delta-byte))

    ready-command
    (do (mark-ready!)
        (@!line-handler :ready))

    pong-command
    (@!line-handler :pong)

    (println :unknown-teensy-message
             {:type type
              :hex (format "0x%02X" type)
              :args args})))

;; -----------------------------
;; Reading framed messages
;; -----------------------------

#_(defn reader-loop!
  [^SerialPort port]
  (try
    (let [^InputStream in (.getInputStream port)]
      (loop []
        (when @!running?
          (let [b (read-byte! in)]
            ;; Hunt for a frame start. Anything before MSG_START (boot-loader
            ;; chatter, partial frames after a reset, etc.) is ignored.
            (when (= b msg-start)
              (let [type (read-byte! in)]
                (if-let [n (message-arg-counts type)]
                  ;; Read exactly the args this type carries, then require the
                  ;; terminator. Counting args by type (rather than reading up
                  ;; to the newline) keeps a raw arg byte that happens to equal
                  ;; MSG_END or MSG_START from corrupting the frame.
                  (let [args (vec (repeatedly n (fn [] (read-byte! in))))
                        end  (read-byte! in)]
                    (if (= end msg-end)
                      (dispatch-message! type args)
                      (println :teensy-frame-unterminated
                               {:type type
                                :args args
                                :got end
                                :hex (format "0x%02X" end)})))
                  (println :unknown-teensy-message
                           {:type type
                            :hex (format "0x%02X" type)})))))
          (recur))))

    (catch Throwable e
      ;; Closing the port intentionally causes the blocking read to break.
      ;; Only print unexpected reader failures.
      (when @!running?
        (println :teensy-reader-error
                 {:message (.getMessage e)
                  :type (class e)})))

    (finally
      (reset! !running? false)
      (reset! !teensy-ready? false))))

(defn reader-loop* [^InputStream in]
  (let [b (read-byte! in)]
    ;; Hunt for a frame start. Anything before MSG_START (boot-loader
    ;; chatter, partial frames after a reset, etc.) is ignored.
    (when (= b msg-start)
      (let [type (read-byte! in)]
        (if-let [n (message-arg-counts type)]
          ;; Read exactly the args this type carries, then require the
          ;; terminator. Counting args by type (rather than reading up
          ;; to the newline) keeps a raw arg byte that happens to equal
          ;; MSG_END or MSG_START from corrupting the frame.
          (let [args (vec (repeatedly n (fn [] (read-byte! in))))
                end  (read-byte! in)]
            (if (= end msg-end)
              (dispatch-message! type args)
              (println :teensy-frame-unterminated
                       {:type type
                        :args args
                        :got end
                        :hex (format "0x%02X" end)})))
          (println :unknown-teensy-message
                   {:type type
                    :hex (format "0x%02X" type)}))))))

(defn reader-loop!
  [^SerialPort port]
  (try
    (let [^InputStream in (.getInputStream port)]
      (loop []
        (when @!running?
          (#'reader-loop* in)
          (recur))))

    (catch Throwable e
      ;; Closing the port intentionally causes the blocking read to break.
      ;; Only print unexpected reader failures.
      (when @!running?
        (println :teensy-reader-error
                 {:message (.getMessage e)
                  :type (class e)})))

    (finally
      (reset! !running? false)
      (reset! !teensy-ready? false))))

(defn start-reader! []
  (if-let [^Thread existing @!reader-thread]
    (if (.isAlive existing)
      :already-started
      (do
        (reset! !reader-thread nil)
        (start-reader!)))

    (let [^SerialPort port (or @!port (open-port!))]
      (when-not (.isOpen port)
        (throw (ex-info "Serial port is not open"
                        {:port (.getSystemPortName port)})))

      (reset! !running? true)

      (let [t (Thread. (fn [] (reader-loop! port))
                       "megasynth-teensy-reader")]
        (.setDaemon t true)
        (.start t)
        (reset! !reader-thread t)

        ;; Ask Teensy to confirm readiness. If it has just rebooted,
        ;; it may also independently print READY.
        (try
          (write-line! "READY?")
          (catch Throwable e
            (println :ready-request-failed
                     {:message (.getMessage e)})))

        :started))))

;; -----------------------------
;; Writing display commands
;; -----------------------------

(defn write-line!
  [s]
  (let [^SerialPort port @!port]
    (when-not (and port (.isOpen port))
      (throw (ex-info "Serial port is not open" {})))

    (let [^OutputStream out (.getOutputStream port)
          bytes (.getBytes (str s "\n") "UTF-8")]
      (.write out bytes)
      (.flush out))))

(defn safe-field [s]
  ;; Protocol rule: no tabs/newlines inside fields.
  ;; Replace them rather than crashing.
  (-> (str s)
      (str/replace #"\t|\n|\r" " ")))

(defn send-display!
  "amount: 0-1000 for bar, -1 for no bar."
  [index label value amount]
  (write-line!
   (str "D"
        "\t" index
        "\t" (safe-field label)
        "\t" (safe-field value)
        "\t" amount)))

(defn clear-displays! []
  (write-line! "CLEAR"))

(defn ping! []
  (write-line! "PING"))

(defn invert-encoder!
  [index invert?]
  (write-line!
   (str "INV"
        "\t" index
        "\t" (if invert? 1 0))))

;; -----------------------------
;; Convenience / testing
;; -----------------------------

(comment

  ;; Strongly recommended after prior experiments:
  ;; restart the REPL/JVM once, so no old raw-byte dump futures
  ;; are still stealing bytes from the serial stream.

  (list-ports)

  (open-port! "ttyACM0")
  
  ;; or maybe:
  (open-port! "ttyACM1")

  (start-reader!)

  ;; Turn a knob. Expected:
  ;; :encoder 0 :delta 1
  ;; :encoder 0 :delta -1

  (ready?)

  (send-display! 0 "MEGASYNTH" "FOREVER" 800)
  (send-display! 1 "BILL" "SUCCESS" 900)

  (invert-encoder! 0 true)

  (clear-displays!)

  (ping!)
  ;; Expected:
  ;; :teensy-line PONG

  (close-port!)

  (comment))
