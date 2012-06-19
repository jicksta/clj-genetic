(use '[clojure.string :only (split trim-newline)])
(use 'clj-time.core)
(use 'clj-time.format)

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

(def csv-contents (slurp "schedule.csv"))

(defn parse-date [date]
  "The CSV expresses flight times as HH:MM. This converts this String into a clj-time DateTime object."
  (parse (formatters :hour-minute) date))

(defn parse-flights []
  "Load each flight from the already-loaded CSV and return a list of flight structs"
  (let [lines (split csv-contents #"\n")] 
    (for [line, lines] 
      (let [[from to departs-at arrives-at cost] (split (trim-newline line) #",")] 
        (struct flight from to (parse-date departs-at) (parse-date arrives-at) (read-string cost))))))

(defn indexed-flights [flights]
  "Groups all flights by [from,to] vectors as the key with the list of matching flight structs as the values."
  (group-by (fn [flight] [(:from flight) (:to flight)]) flights))

;(def sample-solution [[1 4] [3 2] [7 3] [6 3] [2 4] [5 3]])

(def flights (indexed-flights (parse-flights)))

(defn solutions-with-people [solution]
  "Returns a list associating each person with the relevant flights from the way it's expressed in a solution.
   Takes the form (person struct, departure flight struct, arrival flight struct)"
  (for [[person [departure-index, return-index]] (vec (apply map vector [people solution]))]
    (let [origin (:origin person)]
    [person (nth (flights [origin destination]) departure-index) (nth (flights [origin destination]) return-index)])))

(defn schedule-cost [raw-solution]
  "Returns the overall numerical cost of a given solution.
   Used to compare relative betterness of different solutions."
  (let [results (solutions-with-people raw-solution)
        first-departure-time (:departs-at (nth (first results) 1))
        first-arrival-time   (:arrives-at (nth (first results) 2))]
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

(defn random-departure-index-for-person [person]
  (rand-int (count (flights [(:origin person) destination]))))

(defn random-arrival-index-for-person [person]
  (rand-int (count (flights [destination (:origin person)]))))

(defn random-solution []
  (let [random-seq (repeatedly #(rand-int 100))]
  (for [person people]
    [(random-departure-index-for-person person) (random-arrival-index-for-person person)])))

(def random-solutions
  (sort-by schedule-cost (take 100 (repeatedly random-solution))))

(println "Solutions:")
(dorun (for [solution (take 10 random-solutions)]
  (println (schedule-cost solution) (flatten solution))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

;(defn print-schedule [flights solution]
;  (let [[person [departs-index, returns-index]] (interleave people sample-solution)
;         {depart-flight-num :flight-num} (nth flights departs-index)
;         {return-flight-num :flight-num} (nth flights returns-index)]
;    (prn (format
;      "%10s should take Flight #%s and Flight #%s"
;      (:name person) depart-flight-num return-flight-num))))

