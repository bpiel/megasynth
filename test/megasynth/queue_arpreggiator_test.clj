(ns megasynth.queue-arpreggiator-test
  (:require  [clojure.test :as t]
             [megasynth.queue-appregiator :as sut]))


(t/deftest push-note1
  (let [cfg {:long-len 8}] ;; NOTE: 8-length arpeggio uses 5 notes
    (t/is (= (sut/push-note cfg [] 1)
             [1]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 7)
             [1 22 303 4 7]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 303)
             [1 22 4]))
    (t/is (= (sut/push-note cfg [1 22 303 4 55 606 7 88] 909)
             [1 22 303 4 55 606 7 88 909]))
    ;; max vector length is 8 * 2 = 16
    ;; only consider last 5 for removal
    (t/is (= (sut/push-note cfg [1 12 103 224 15 106 7 18 109 10 11 22 203 224 55 206] 224)
             [1 12 103 224 15 106 7 18 109 10 11 22 203 55 206]))
    ;; only consider last 5 for removal
    (t/is (= (sut/push-note cfg [0 1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206] 4)
             [1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206 4]))
    ;; only consider last 5 for removal
    (t/is (= (sut/push-note cfg [1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206] 55)
             [1 12 103 4 15 106 7 18 109 10 11 22 203 224 206])))
  (let [cfg {:long-len 6}] ;; NOTE: 6-length arpeggio uses 4 notes
    (t/is (= (sut/push-note cfg [] 1)
             [1]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 7)
             [1 22 303 4 7]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 303)
             [1 22 4]))
    (t/is (= (sut/push-note cfg [1 22 303 4 55 606 7 88] 909)
             [1 22 303 4 55 606 7 88 909]))
    ;; max vector length is 6 * 2 = 12
    ;; only consider last 4 for removal
    (t/is (= (sut/push-note cfg [1 12 103 224 15 106 7 18 109 10 11 22 224 203 55 206] 224)
             [1 12 103 224 15 106 7 18 109 10 11 22 203 55 206]))
    ;; only consider last 4 for removal
    (t/is (= (sut/push-note cfg [0 1 12 103 4 15 106 7 18 109 10 11] 4)
             [1 12 103 4 15 106 7 18 109 10 11 4]))
    ;; only consider last 4 for removal
    (t/is (= (sut/push-note cfg [0 1 12 103 4 15 106 7 18 109 10 11] 12)
             [1 12 103 4 15 106 7 18 109 10 11 12]))))



;; UGH!!!! I FORGOT SORTING!!

(t/deftest gen-seq1
  (let [cfg {:short-len 4 :long-len 8}]
    (t/is (= (sut/gen-seq cfg [])
             []))
    (t/is (= (sut/gen-seq cfg [11])
             [11 11 11 11]))
    (t/is (= (sut/gen-seq cfg [11 22])
             [11 22 22 22]))    
    (t/is (= (sut/gen-seq cfg [11 22 13])
             [11 13 22 13]))
    (t/is (= (sut/gen-seq cfg [11 22 44 13])
             [11 13 22 44 22 13 22 13]))
    (t/is (= (sut/gen-seq cfg [22 11 13 44 15])
             [11 13 15 22 44 22 15 13]))
    (t/is (= (sut/gen-seq cfg [13 11 22 44 66 15])
             [11 15 22 44 66 44 22 15]))
    (t/is (= (sut/gen-seq cfg [77 16 5 14 33 12 1])
             [1 5 12 14 33 14 12 5]))))

