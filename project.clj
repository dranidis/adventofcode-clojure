(defproject advent-2023 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clojure-term-colors "0.1.0"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/data.priority-map "1.2.0"]
                 [criterium "0.4.6"]
                 [com.clojure-goes-fast/clj-async-profiler "1.5.1"]]
  :repl-options {:init-ns advent-2023.core}
  :jvm-opts ["-Xss4m"
             "-Djdk.attach.allowAttachSelf"])
