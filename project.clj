(defproject general-clojure "A version"
  :description "My general clojure things"
  :license {:name "MIT license"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :plugins [[lein-kibit "0.0.8"] [jonase/eastwood "0.2.1"] [lein-bikeshed "0.2.0"]]
  :aliases {"omni" ["do" ["clean"] ["with-profile" "user" "deps" ":tree"] ["kibit"] ["bikeshed"]]}
  :main ^:skip-aot general-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
