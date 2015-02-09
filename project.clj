(defproject general-clojure "A version"
  :description "My general clojure things"
  :license {:name "MIT license"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot general-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
