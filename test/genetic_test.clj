(ns genetic-test
  (:use
    genetic
    lazytest.describe
    [clojure.set :only (subset?)])
  (:require
    clojure.set))

; TODO: Setup mocking framework so I can test hill-search

(defn throws? [fn & args]
  (try
    (apply fn args)
    false
    (catch Error e true)))

(def twice-people-count (* 2 (count people)))

(describe throws?
  (it "returns true when the given function raises an exception"
    (true? (throws? #(assert false))))
  (it "returns false when the given function does not raise an exception"
    (false? (throws? (fn [] :not-throwing)))))

(defn aggregate-costs-
  "Pass in either clojure.core/max or clojure.core/min as a fn
  to get the max or min of all the costs per person"
  [aggregation-fn]
  (*
    twice-people-count
    (apply aggregation-fn (map :cost (parse-flights)))))

(describe csv-contents
  (it "has five tokens in its first line"
    (= 5 (count (first csv-contents))))

  (it "returns a sequence containing a list of tokens for each line"
    (let [fields-in-line (first csv-contents)]
      (every?
        #(re-matches #"[\w:]+" %)
        fields-in-line))))

(describe conj-present
  (it "does not conj args that are falsy"
    (let [returned-set (conj-present #{1} nil 2 nil 3)]
      (and
        (not (contains? returned-set nil))
        (contains? returned-set 1)
        (contains? returned-set 2)
        (contains? returned-set 3)))))

(describe schedule-cost
  (it "returns a number within the minimum and maximum costs for a solution"
    (let [cost (schedule-cost (random-solution))
          max-cost (aggregate-costs- max)
          min-cost (aggregate-costs- min)]
      (and
        (< cost max-cost)
        (> cost min-cost)))))

(describe flights-count
  (it "returns a number for a given person in a certain direction"
    (and
      (< 0 (flights-count (first people) :depart))
      (< 0 (flights-count (first people) :arrive)))))

(describe person-for-index
  (it "returns the right person at each index"
    (=
      (map person-for-index (range twice-people-count))
      (reduce #(conj %1 %2 %2) [] people))))

(describe direction-for-index
  (it "returns the correct directon for all possible indices"
    (=
      (take twice-people-count (cycle [:depart :arrive]))
      (map direction-for-index (range twice-people-count)))))

(describe solutions-with-people
  (it "returns a list containing a collection with the person and
       the departure and arrival flights"
    (let [solution (random-solution)
          [first-result & _ :as result] (solutions-with-people solution)
          [person depart-flight return-flight] first-result
          {origin :origin} person]
      (and
        (= person (first people))
        (= depart-flight (flight-at-index origin destination (nth solution 0)))
        (= return-flight (flight-at-index destination origin (nth solution 1)))))))

(describe adjacent-solutions
  (it "returns a set of adjacent solutions"
    (let [solution (take twice-people-count (repeat 1))
          adjacents (adjacent-solutions solution)
          known-adjacents #{ ; More would be better
                            (change-index solution 0 0)
                            (change-index solution 0 2)
                            (change-index solution 1 0)
                            (change-index solution 1 2)}]
      (subset? known-adjacents adjacents))))


(describe random-generation
  (it "returns an array of solutions"
    (let [generation (random-generation 10)]
      (every?
        (fn [solution]
          (and
            (= twice-people-count (count solution))
            (every? #(= % (int %)) solution))) generation))))

(describe next-generation

  (let [subsequent-generation (nth (iterate next-generation (random-generation 10)) 3)]

    (it "returns a two-dimensional collection of collections"
      (let [[first-solution & _ :as first-generation] subsequent-generation]
        (and
          (every? coll? first-generation)
          (every? #(= % (int %)) (wtf first-solution)))))
    (it "returns the same number of population as the given generation"
      (= 10 (count subsequent-generation)))))


(describe indexed-flights
  (it "keys the map with lists containing the from and to airport codes"
    (let [origin "FOO"
          destination "BAR"
          index (indexed-flights (list {:from origin :to destination}))] 
      (=
        (first (keys index))
        (list origin destination)))))

(describe random-solution
  (it "returns two numbers per person"
    (= (count (random-solution)) (* 2 (count people)))))

(describe mutate
  (letfn
    [(check-mutation []
      (let [solution (random-solution)
            mutated (mutate solution)]
        (and
          (not-any? nil? mutated)
          (= 1 (count
                 (filter false?
                   (map = solution mutated)))))))]

    (it "changes one of the numbers in the solution to a different random value"
      (every? identity (take 100 (repeatedly check-mutation))))))
