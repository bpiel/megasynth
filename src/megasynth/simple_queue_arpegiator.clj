(ns megasynth.simple-queue-arpegiator)

(defn push-note [_cfg q note]
  (vec (take-last 5 (conj (or q []) note))))

(defn gen-seq* [sorted]
  (let [n (count sorted)]
    (vec (case n
           1 sorted
           2 (take 1 sorted)
           (drop-last sorted)))))

(defn gen-seq [_cfg q]
  (if (empty? q)
    []
    (let [sorted (vec (sort q))
          fwd (take 4 sorted) #_(gen-seq* sorted)
          bwd (->> sorted reverse (take 4)) #_(-> sorted reverse gen-seq*)]
      (->> (concat fwd bwd)
           #_cycle
           #_(take 8)
           vec))))
