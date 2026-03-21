(ns megasynth.queue-appregiator)

(defn push-note [{:keys [long-len] :as cfg} q note]
  (let [n (count q)
        ;; Removal window is (long-len / 2) + 1
        window-size (+ (quot long-len 2) 1)
        ;; Check if note is in last window-size (or whole queue if n <= window-size)
        window-start (max 0 (- n window-size))
        window (subvec q window-start)
        in-window? (some #(= % note) window)]
    (if in-window?
      ;; Remove from window only (keep earlier occurrences), NO trimming
      (let [idx-in-window (.indexOf window note)
            idx-in-q (+ window-start idx-in-window)]
        (vec (concat (subvec q 0 idx-in-q)
                     (subvec q (inc idx-in-q)))))
      ;; Not in window: add it, trim if at or over max
      (let [q-trimmed (if (>= n (* 2 long-len))
                        (vec (rest q))
                        q)]
        (conj q-trimmed note)))))

(defn gen-seq [{:keys [short-len long-len] :as cfg} q]
  (let [sorted (vec (sort q))
        n (count sorted)]
    (cond
      (= n 0) []
      (= n 1)
      ;; Single element: repeat to fill short-len
      (vec (repeat short-len (nth sorted 0)))
      (< n short-len)
      ;; Pad with element at index 1 to reach exactly short-len
      (let [pad-elem (nth sorted 1)
            padding (repeat (- short-len n) pad-elem)]
        (vec (concat sorted padding)))
      (= n short-len)
      ;; Exactly short-len: bounce between indices (short-len-2) and 1
      (let [remaining (- long-len n)
            indices (take remaining (cycle [(- short-len 2) 1]))]
        (vec (concat sorted (map #(nth sorted %) indices))))
      :else
      ;; Queue exceeds short-len: take last 5, sort, apply arpeggio 0 1 2 3 4 3 2 1
      (let [last-5 (vec (sort (take-last 5 q)))
            arpeggio-indices [0 1 2 3 4 3 2 1]]
        (vec (map #(nth last-5 %) arpeggio-indices))))))
