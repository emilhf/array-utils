(ns array-utils.generative.tests
  (:require [clojure.test.generative :as test :refer (defspec)]
            [clojure.data.generators :as gen]
            [clojure.test.generative.event :as event] ;; UI console
            [clojure.test.generative.runner :as runner]
            [array-utils.double :as d]
            [array-utils.long :as l]))

(defn double? [n] (instance? Double n))

(defn long? [n] (instance? Long n))

(defn even? [n] (zero? (mod n 2)))

(def double1 (double-array (range 500 1000)))

(def long1 (long-array (range 500 1000)))

(defn dot-product-long [ws xs] (l/asum [w ws x xs] (* w x)))

(defn dot-product-double [ws xs] (d/asum [w ws x xs] (* w x)))

;; # Generators

(defn darray
  ([] (darray 10e3))
  ([size]
      (gen/double-array gen/double size)))

(defn larray
  ([] (larray 10))
  ([size] (larray size 0 10e3))
  ([size hi lo]
     (gen/long-array (gen/uniform hi lo) size)))

;; # Tests

;; ----------------------------------------------------------------------

;; ## Doubles

(defspec dot-product-returns-double
  dot-product-double
  [^{:tag (`darray 10e3)} a ^{:tag (`darray 10e3)} b] 
  (assert (double? %)))

(defspec amean-double-returns-mean
  d/amean
  [^{:tag (`darray 10e3)} a] 
  (assert (= (/ (reduce + a) (count a)) %)))

(defspec afilter-returns-even-doubles
  (fn [a] (d/afilter even? a)) 
  [^{:tag (`darray 10e3)} a] 
  (assert (every? even? %)))

(defspec amax-returns-the-largest-double
  d/amax
  [^{:tag (`darray 10e3)} a] 
  (assert (= (reduce max a) %)))

(defspec amin-returns-the-smallest-double
  d/amin
  [^{:tag (`darray 10e3)} a] 
  (assert (= (reduce min a) %)))

;; ----------------------------------------------------------------------

;; ## Longs

(defspec dot-product-returns-long
  dot-product-long
  [^{:tag (`larray 100 0 10e3)} a ^{:tag (`larray 100 0 10e3)} b] 
  (assert (long? %)))

(defspec amean-long-returns-mean
  l/amean
  [^{:tag (`larray 10e3)} a] 
  (assert (= (/ (reduce + a) (count a)) %)))

(defspec afilter-returns-even-longs
  (fn [a] (l/afilter even? a)) 
  [^{:tag (`larray 10e3)} a] 
  (assert (every? even? %)))

(defspec amax-returns-the-largest-long
  l/amax 
  [^{:tag (`larray 10e3)} a] 
  (assert (= (reduce max a) %)))

(defspec amin-returns-the-smallest-long
  l/amin 
  [^{:tag (`larray 10e3)} a] 
  (assert (= (reduce min a) %)))

;; TODO: write tests for afill!, doarr, collect
 
(defn -main []
  (runner/-main "."))

;; (comment

;;   (event/install-default-handlers)

;;   
  
;;   (runner/-main "test/array_utils/generative")

;;   )