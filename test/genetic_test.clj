(ns genetic-test
  (:use genetic
        lazytest.describe))

(describe "The bar namespace"
  (it "returns foo"
    (re-matches #".*foo.*" (foo))))
