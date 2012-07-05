(defproject genetic "0.1"
  :description "Experimenting with genetic algorithms in Clojure"
  :url "http://github.com/jicksta/clj-genetic"
  :main genetic/main
  :dependencies [
    [org.clojure/clojure "1.3.0"]
    [org.clojure/clojure-contrib "1.2.0"]
    [org.clojure/tools.trace "0.7.3"]
    [com.stuartsierra/lazytest "2.0.0-SNAPSHOT"]
    [clj-time "0.4.3"]
  ]
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"})
