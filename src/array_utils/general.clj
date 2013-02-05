(ns ^{:doc
      "Utility macros and functions to make working with primitive
       double arrays a bit more bearable. Very alpha and somewhat
       ugly. Use with care."
      :author "EHF"}
  array-utils.core
  (:refer-clojure :exclude [amap])
  (:require [criterium.core :as bench])
  ) 

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)
;; enable on pain of (REPL) death
(set! *print-length* 15) 

;; # Experimental, decoupled version of `array-utils`. Currently
;; # implemented for doubles and longs. Easily extended to int, float
;; # et cetera.

;; ## Utils

;; Accepts singular and plural forms of the casting function, for
;; example [long longs]. This would have been much nicer using a map,
;; but map destructuring doesn't seem to work in macros. When a set-fn
;; is required, it's usually aset-double, aset-long etc.

(defmacro abind-hint
  [[sg pl] bindings i & body]
  `(let ~(reduce
          (fn [init [name arr]]
            (if-not (symbol? name)
              (conj init name (vector `(~sg ~i)
                                      `(aget (~pl ~arr) ~i)))
              (conj init name `(aget (~pl ~arr) ~i))))
          [] (partition 2 bindings))
     ~@body))


(defmacro reduce-with-hint
  [[sg pl] [f unit] bindings & body]
  (let [arr `(~pl ~(second bindings))] ;; manual type hints = 1000x perf
    `(areduce ~arr i# ret# ~unit
              (~f ret#
                  (abind-hint [~sg ~pl] ~bindings i# ~@body)))))

(defmacro doarr-hint
  [[sg pl] bindings & body]
  (let [arr `(~pl ~(second bindings))]
    `(dotimes [i# (alength ~arr)]
       (abind-hint [~sg ~pl] ~bindings i# ~@body))))

(defmacro amap-hint
  [[sg pl] binding & body]
  (let [arr `(~pl ~(second binding))] ;; doubleize => 1000x perf
    `(clojure.core/amap ~arr i# ret#
                        (~sg (abind-hint [~sg ~pl] ~binding i# ~@body)))))

(defmacro afill-hint!
  [[sg pl set-fn] bindings & body]
  (if (symbol? (first bindings))
    `(afill-hint! [~sg ~pl ~set-fn]
                  [[i# ~(first bindings)] ~(last bindings)] ~@body)
    `(doarr-hint [~sg ~pl]
                 ~bindings
                 (~set-fn ~@((juxt last ffirst) bindings) ~@body))))

;; ### Long implementations

(defmacro reduce-with-long
  [[f unit] bindings & body]
  `(reduce-with-hint [long longs] [~f ~unit] ~bindings ~@body))

(defmacro amap-long [bindings & body]
  `(amap-hint [long longs] ~bindings ~@body))

(defmacro doarr-long [bindings & body]
  `(doarr-hint [long longs] ~bindings ~@body))

(defmacro afill-long! [bindings & body]
  `(afill-hint! [long longs aset-long] ~bindings ~@body))

(defn afilter-long
  ([pred array]
     (let [acc (transient [])]
       (doarr-long [a array]
                   (when (pred a)
                     (conj! acc a)))
       (long-array (persistent! acc))))
  ([pred array unit]
     (amap-long [a array] (if (pred a) a unit))))

(defn afilter-double!
  [pred array unit]
  (afill-long! [a array] (if (pred a) a unit)))

(defmacro aproduct-long
  ([array]
     `(aproduct-long [a# ~array] a#))
  ([bindings & body]
     `(reduce-with-long [* (long 1)]
        ~bindings ~@body)))

(defmacro asum-long
  ([array]
     `(asum-long [a# ~array] a#))
  ([bindings & body]
     `(reduce-with-long [+ (long 0)]
        ~bindings ~@body)))

(defn collect-long
  [f unit xs]
  (reduce-with-long [f unit]
    [x xs] x))

(defn amax-long [xs] (collect-long max Long/MIN_VALUE xs))

(defn amin-long [xs] (collect-long min Long/MAX_VALUE xs))

(defn amean-long [xs] (/ (asum-long xs) (alength ^longs xs)))

;; # Double implementations

(defmacro reduce-with-double
  [[f unit] bindings & body]
  `(reduce-with-hint [double doubles] [~f ~unit] ~bindings ~@body))

(defmacro amap-double [bindings & body]
  `(amap-hint [double doubles] ~bindings ~@body))

(defmacro doarr-double [bindings & body]
  `(doarr-hint [double doubles] ~bindings ~@body))

(defmacro afill-double! [bindings & body]
  `(afill-hint! [double doubles aset-double] ~bindings ~@body))

(defn afilter-double
  ([pred array]
     (let [acc (transient [])]
       (doarr-double [a array]
                     (when (pred a)
                       (conj! acc a)))
       (double-array (persistent! acc))))
  ([pred array unit]
     (amap-double [a array] (if (pred a) a unit))))

(defn afilter-double!
  [pred array unit]
  (afill-double! [a array] (if (pred a) a unit)))

(defmacro aproduct-double
  ([array]
     `(aproduct-double [a# ~array] a#))
  ([bindings & body]
     `(reduce-with-double [* (double 1)]
        ~bindings ~@body)))

(defmacro asum-double
  ([array]
     `(asum-double [a# ~array] a#))
  ([bindings & body]
     `(reduce-with-double [+ (double 0)]
        ~bindings ~@body)))

(defn collect-double
  [f unit xs]
  (reduce-with-double [f unit]
    [x xs] x))

(defn amax-double [xs] (collect-double max Double/MIN_VALUE xs))

(defn amin-double [xs] (collect-double min Double/MAX_VALUE xs))

(defn amean-double [xs] (/ (asum-double xs) (alength ^doubles xs)))