(defproject array-utils "0.1.0-ALPHA"
  :description "Utility macros and functions to make working with
                primitive double arrays a bit more bearable."
  :url "http://vinjeboy.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0-RC4"]
                 [org.apache.commons/commons-math3 "3.0"]
                 [criterium "0.3.0"]
                 [prismatic/plumbing "0.0.1"]]
  :profiles {:dev {:plugins [[lein-marginalia "0.7.1"]]}}
  :warn-on-reflection true)
