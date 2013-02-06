(ns ^{:doc "Benchmarking suite for array-utils"
      :author "EHF"}
  array-utils.benchmark 
  (:use clojure.test)
  (:require [array-utils.double :as d]
            [array-utils.long :as l]
            [criterium.core :as bench]))

;; # Utils and setup

;; Very unsafe, but to hell with it!
(defn gen-fname []
  (let [now (java.util.Date.)]
    (clojure.string/replace (str "benchmarks/" now ".txt") #"\s|\d\d:\d\d:\d\d" "")))

(def doubles1 (double-array (range 500 1500)))
(def doubles2 (double-array (repeatedly 10e2 rand)))

(def longs1 (long-array (range 500 1500)))
(def longs2 (long-array (repeatedly 10e2 #(rand-int 500))))

;; ----------------------------------------

;; # Benchmarking suite. Go wild!

;; TODO: Port over solutions from Alioth?

(defn dot-product-double [ws xs]
  (d/asum [w ws x xs] (* w x)))

(defn dot-product-long [ws xs]
  (l/asum [w ws x xs] (* w x)))

;; ----------------------------------------

(def line (apply str (repeat 72 "=")))

(defmacro run-bench* [expr]
  `(do (println (str "Testing the expression: " (str ~expr)))
       (bench/quick-bench ~expr)
       (println line)))

(defmacro run-bench [& exprs]
  `(doseq [expr# '~exprs]
     (run-bench* expr#)))

(defn run-benchmarks
  "Simple functions should be quick-checked and large, complicated
  operations be benched." []
  (with-out-str 
    (run-bench
     (dot-product-double doubles1 doubles1)
     (dot-product-long longs1 longs1))
    ;; (bench/bench (solve-world-hunger))
    ))

(defn -main []
  (println "Benchmarking. This might take a while.")
  (let [res (run-benchmarks)
        fname (gen-fname)]
    (spit fname res :append true)
    (println (str "Done. Results stored to " fname))))