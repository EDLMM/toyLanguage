(defproject language0 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                  [instaparse "1.4.9"]
                  [rhizome "0.2.9"];;visualization
                ]
  :main ^:skip-aot language0.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
