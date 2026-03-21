(ns megasynth.queue-arpreggiator-test
  (:require  [clojure.test :as t]
             [megasynth.queue-appregiator :as sut]))



(t/deftest push-note1
  (let [cfg {:short-len 4 :long-len 8}]
    (t/is (= (sut/push-note cfg [] 1)
             [1]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 7)
             [1 22 303 4 7]))
    (t/is (= (sut/push-note cfg [1 22 303 4] 303)
             [1 22 4]))
    (t/is (= (sut/push-note cfg [1 22 303 4 55 606 7 88] 909)
             [1 22 303 4 55 606 7 88 909]))
    (t/is (= (sut/push-note cfg [1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206] 123)
             [12 103 4 15 106 7 18 109 10 11 22 203 224 55 206 123]))
    (t/is (= (sut/push-note cfg [1 12 103 4 15 106 7 18 109 10 11 22 203 224 55 206] 4)
             [12 103 15 106 7 18 109 10 11 22 203 224 55 206 123]))))
