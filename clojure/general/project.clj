(defproject general-clojure "A version"
  :description "My general clojure things"
  :license {:name "MIT license"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :plugins [[lein-pprint "1.1.1"] [lein-ancient "0.6.2"] [lein-kibit "0.0.8"] [jonase/eastwood "0.2.1"] [lein-bikeshed "0.2.0"]]
  :aliases {"omni" ["omni-raw" "with-profile" "general-clojure"] "omni-raw" ["do" ["deps" ":tree"] "bikeshed" "kibit" "ancient"]}
  :main ^:skip-aot general-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
