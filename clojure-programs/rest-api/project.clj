(defproject rest-api "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [compojure "1.7.0"]
                 [http-kit "2.3.0"]
                 [ring/ring-defaults "0.3.4"]
                 [org.clojure/data.json "2.4.0"]
                 [cheshire "5.13.0"]
                 [com.github.seancorfield/next.jdbc "1.3.834"]
                 [org.postgresql/postgresql "42.2.10"]
                 [com.github.seancorfield/honeysql "2.3.928"]]
  :main ^:skip-aot rest-api.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
