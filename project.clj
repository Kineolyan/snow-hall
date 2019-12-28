(defproject snow-hall "0.1.0-SNAPSHOT"
  :description "Game Server for little games."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.6.1"]
                 [http-kit "2.3.0"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-json "0.5.0"]]
  :main ^:skip-aot snow-hall.core
  :target-path "target/%s"
  :source-paths ["src/clojure"]
  :test-paths ["test/clojure"]
  :java-source-paths ["src/java"]
  :javac-options ["-target" "11" "-source" "11"]
  :profiles {
             :uberjar {:aot :all}
             :test {
                    :dependencies [[org.clojure/data.json "0.2.7"]]
                    :test-selectors {:default (complement :integration)
                                     :integration :integration}}})
