(ns genetic
  (:use
    ;[fs.core :only (normalized-path)]
    [clojure.string :only (split trim-newline)]
    [clojure.java.io :only (reader)]
    [clj-time.core :only (before? after?)]
    [clj-time.format :only (parse formatters)])
  (:require clojure.java.io clojure.set))

(defstruct person :name :origin)
(defstruct flight :from :to :departs-at :arrives-at :cost)

(def destination "LGA")

(def people [
   (struct person "Seymour" "BOS")
   (struct person "Franny"  "DAL")
   (struct person "Zooey"   "CAK")
   (struct person "Walt"    "MIA")
   (struct person "Buddy"   "ORD")
   (struct person "Les"     "OMA")
])

(def csv-contents (partition 5 (split (slurp "data/schedule.csv") #"[,\n]")))

(defn wtf
  "Just my little inspect helper"
  [& vals]
  (apply println vals)
  (last vals))

(defn parse-date
  "The CSV expresses flight times as HH:MM. This converts this String into a clj-time DateTime object."
  [date]
  (parse (formatters :hour-minute) date))

(defn parse-flights
  "Load each flight from the already-loaded CSV and return a list of flight structs"
  []
  (for [line csv-contents]
    (let [[from to departs-at arrives-at cost] line]
      (struct flight from to (parse-date departs-at) (parse-date arrives-at) (read-string cost)))))

(defn indexed-flights
  "Groups all flights by [from,to] vectors as the key with the
   list of matching flight structs as the values."
  [flights]
  (group-by (fn [flight] [(:from flight) (:to flight)]) flights))

#_(def sample-solution [1 4 3 2 7 3 6 3 2 4 5 3])

(def flights (indexed-flights (parse-flights)))

(defn change-index
  "Returns a new collection with the given index changed to a new value"
  [coll index new-value]
  (concat (take index coll) [new-value] (nthnext coll (inc index))))

(defn flight-at-index
  "Returns the flight at a given index from one airport to another"
  [from to index]
  ((flights [from to]) index))

(defn solutions-with-people
  "Returns a list associating each person with the relevant flights from the way it's expressed
  in a solution. Takes the form (person struct, departure flight struct, arrival flight struct)"
  [solution]
  (for [[person [departure-index, return-index] :as ohai] (map list people (partition 2 solution))]
    (let [origin (:origin person)]
      (vector
        person
        (flight-at-index origin destination departure-index)
        (flight-at-index destination origin return-index)))))

(defn schedule-cost
  "Returns the overall numerical cost of a given solution.
   Used to compare relative betterness of different solutions."
  [raw-solution]
  (let [results (solutions-with-people raw-solution)
        first-departure-time (:departs-at ((first results) 1))
        first-arrival-time   (:arrives-at ((first results) 2))]
    (loop [[[person departure return] & remaining] results
           total-cost 0
           earliest-departure first-departure-time
           latest-arrival first-arrival-time]
      ; TODO: Support wait times.
      (let [new-cost (+ total-cost (:cost departure) (:cost return))]
       (if (empty? remaining)
         new-cost
         (recur
           remaining
           new-cost
           (if (before? (:departs-at departure) earliest-departure)
             (:departs-at departure)
             earliest-departure)
           (if (after? (:arrives-at return) latest-arrival)
             (:arrives-at departure)
             latest-arrival)))))))

(defn flights-count
  "Returns the number of available flights for the given person in a certain direction"
  [person direction]
  (let [direction-transform (case direction :depart identity, :arrive reverse)
         airports (direction-transform [(:origin person) destination])]
    (count (flights airports))))

(defn random-departure-index-for-person [person]
  (rand-int (flights-count person :depart)))

(defn random-arrival-index-for-person [person]
  (rand-int (flights-count person :arrive)))

(defn random-solution []
  (reduce concat
    (for [person people]
      (list (random-departure-index-for-person person) (random-arrival-index-for-person person)))))

(defn random-solutions
  ([] (random-solutions 100))
  ([n] (sort-by schedule-cost (take n (repeatedly random-solution)))))

(defn conj-present
  "Conj only params that are truthy"
  [coll & xs]
  (apply conj coll (filter identity xs)))

(defn adjacent-solutions [solution]
  (loop [solutions #{}
         [[value index] & remaining-indices] (map vector solution (range (count solution)))]
    (let [person (people (int (/ index 2)))
          direction (case (mod index 2) 0 :depart, 1 :arrive)
          num-flights (flights-count person direction)
          expanded-solutions (conj-present
                               solutions
                               (when-not (= value (dec num-flights))
                                 (change-index solution index (inc value)))
                               (when-not (zero? value)
                                 (change-index solution index (dec value))))]
      (if (empty? remaining-indices)
        expanded-solutions
        (recur expanded-solutions remaining-indices)))))

(defn better-solution
  "Returns the better of two solutions."
  [one two]
  (if (< (schedule-cost one) (schedule-cost two)) one two))

(defn best-solution
  "Returns the best of all given solutions"
  [solutions]
  (reduce better-solution solutions))

(defn best-adjacent-solution
  "Finds the adjacent solutions which are cheaper than the given solution.
   Returns the given solution if there are no better solutions."
  [solution]
  (let [possible-solutions (conj (adjacent-solutions solution) solution)]
    (best-solution possible-solutions)))

(defn hill-search [start-solution]
  (loop [solution start-solution]
    (let [best-neighbor (best-adjacent-solution solution)]
      (if (= solution best-neighbor)
        solution
        (recur best-neighbor)))))

(defn print-solution
  "Prints the solution with its cost prettily."
  [solution]
  (printf "$%d => %s\n" (schedule-cost solution) (pr-str solution)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: Best solution I've seen so far is $1,573.

(let [n 10, solutions (random-solutions n)]
  (println "Best of" n "solutions generated randomly:")
  (print-solution (best-solution solutions))

  (newline)

  (println "Best of all neighbors of previous set:")
  (print-solution (best-solution (map best-adjacent-solution solutions)))

  (newline)

  (println "Best of hill search:")
  (print-solution (best-solution (map hill-search solutions))))
