(defproject aoc "0.1.0-SNAPSHOT"
  :description "Advent of code"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/algo.generic "0.1.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.async "0.6.532"]
                 [org.clojure/data.json "0.2.7"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/tools.namespace "1.3.0"]
                 [criterium "0.4.6"]
                 [potemkin "0.4.6"]
                 [quil "3.1.0"]
                 [clj-http "3.10.0"]
                 [instaparse "1.4.10"]]
  :main ^:skip-aot aoc.core
  :test-paths ["src"]
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true, *print-length* 20}
  :profiles {:uberjar {:aot :all}})
