(ns megasynth.simple-queue-arpreggiator-test
  (:require  [clojure.test :as t]
             [megasynth.simple-queue-appregiator :as sut]))


(t/deftest push-note1
  (let [cfg {:long-len nil}] ;; NOTE: 8-length arpeggio uses 5 notes
    (t/is (= (sut/push-note cfg [] 1)
             [1]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 7)
             [1 22 303 4 7]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 303)
             [1 22 303 4 303]))
    (t/is (= (sut/push-note cfg [1 22 303 4 55 606 7 88] 909)
             [55 606 7 88 909]))
    ;; max vector length is 8 * 2 = 16
    ;; only consider last 5 for removal
    (t/is (= (sut/push-note cfg [1 12 103 224 15 106 7 18 109 10 11 22 203 224 55 206] 224)
             [203 224 55 206 224]))
    ;; only consider last 5 for removal
    (t/is (= (sut/push-note cfg [0 1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206] 4)
             [203 224 55 206 4]))
    ;; only consider last 5 for removal
    (t/is (= (sut/push-note cfg [1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206] 55)
             [203 224 55 206 55]))))



(t/deftest gen-seq1
  (let [cfg nil]
    (t/is (= (sut/gen-seq cfg [])
             []))
    (t/is (= [11 11 11 11 11 11 11 11]
             (sut/gen-seq cfg [11])))
    (t/is (= [11 22 11 22 11 22 11 22]
             (sut/gen-seq cfg [11 22])))    
    (t/is (= [11 13 22 13 11 13 22 13]
             (sut/gen-seq cfg [11 22 13])))
    (t/is (= [11 13 22 44 22 13 11 13]          
             (sut/gen-seq cfg [11 22 44 13])))
    (t/is (= [11 13 15 22 44 22 15 13]
             (sut/gen-seq cfg [22 11 13 44 15])))
    (t/is (= [11 13 15 22 44 66 44 22]
             (sut/gen-seq cfg [13 11 22 44 66 15])))
    (t/is (= [1 5 12 14 16 33 77 33]          
             (sut/gen-seq cfg [77 16 5 14 33 12 1])))))

