(ns bar-test
  (:use genetic lazytest.describe))

(describe csv-contents
  (it "has five tokens in its first line"
    (= 5 (count (first csv-contents))))
  (it "returns a sequence containing a list of tokens for each line"
    (let [fields-in-line (first csv-contents)]
        (every? #(re-matches #"[\w:]+" %) fields-in-line))))

(describe indexed-flights
  (it "contains keys the map by a list containing the from and to airport codes"
    (let [origin "FOO"
          destination "BAR"
          index (indexed-flights (list {:from origin :to destination}))] 
      (= (first (keys index)) (list origin destination)))))
