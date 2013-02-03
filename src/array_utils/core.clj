(ns ^{:doc
      "Utility macros and functions to make working with primitive
       double arrays a bit more bearable. Very alpha and somewhat
       ugly. Use with care."
      :author "EHF"}
  array-utils.core
  (:refer-clojure :exclude [amap])) 

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)
;; enable on pain of (REPL) death
(set! *print-length* 15) 

;; # Motivation

;; I wrote these functions to deal with some geomechanics related
;; numerics and learn macros. Nothing fancy. Inspired by Prismatic's
;; slides on Flop, which has yet to be released.

;; Benchmarks suggest that these abstractions are only slightly slower
;; than writing primitive operations using areduce and amap, if any
;; slower at all. Which is nice, of course.

;; ## Warnings
;;
;; 1. The macros are somewhat coupled to double arrays. Also, due to
;; JVM black magic, optimalisation is somewhat of a dark art.
;;
;; 2. Composition isn't guaranteed in all circumstances. Please
;; consider using generic, built-in functions for numerics unless you
;; need the raw speed of Java primitives.

;; TODO? Decouple and allow for longs

(defn double-hint
  "Helper function to tag a double array in a macro."
  [obj] (with-meta obj {:tag "[D"}))

(defmacro abind
  "Given bindings of the form `[[idx var] array]`, binds `idx` to the
  current index and `var` to the value at that index. If only `var` is
  given, binds it to the value at the current index. 

  ex. `(arr-bind [a alpha b beta] 2 (* 2 a b)` is rewritten to
  `(let [a (aget alpha 2) b (aget beta 2)] (* 2 a b))`."
  [bindings i & body]
  `(let ~(reduce
          (fn [init [name arr]]
            (if-not (symbol? name)
              (conj init name (vector `(double ~i)
                                      `(aget ~(double-hint arr) ~i)))
              (conj init name `(aget ~(double-hint arr) ~i))))
          [] (partition 2 bindings))
     ~@body))

;; TODO: decouple from doubles?
(defmacro reduce-with-monoid
  "A specialized case of areduce, applying a (at least) two-arity f to
  the accumulator (initialized to unit) and the result of the body.
  Thus, it forms a monoid. See abind for how the bindings work."
  [[f unit] bindings & body]
  (let [arr (double-hint (second bindings))] ;; manual type hints = 1000x perf
    `(areduce ~arr i# ret# ~unit
              (~f ret#
                  (abind ~bindings i# ~@body)))))

;; ## Iterators and higher-order functions

(defmacro arrseq
  "Like doseq, but for arrays. See abind for how the bindings work."
  [bindings & body]
  (let [arr (double-hint (second bindings))]
    `(dotimes [i# (alength ~arr)]
       (abind ~bindings i# ~@body))))

(defmacro amap
  "Mimicks for-each.

  Example usage: `(amap [a natural-numbers] (+ a 2)` is rewritten to
  <pre><code>(core/amap natural-numbers idx ret
  (let [a (get natural-numbers idx)]
    (+ a 2)))</code></pre>"
  [binding & body]
  (let [arr (double-hint (second binding))] ;; doubleize => 1000x perf
    `(clojure.core/amap ~arr i# ret#
                        (double (abind ~binding i# ~@body)))))

(defmacro afill!
  "Example usage: `(afill! [[j v] n] (+ j 2)` adds two to each element
  in the array, doing in-place replacement. See abind for
  information on the bindings work."
  [bindings & body]
  (if (symbol? (first bindings))
    `(afill! [[i# ~(first bindings)] ~(last bindings)] ~@body)
    `(arrseq ~bindings
             (aset-double ~@((juxt last ffirst) bindings) ~@body))))

(defn afilter
  "Returns an array with all items that match the predicate. Accepts
  an optional unit element that is inserted when an element does not
  match the predicate, which improves performance in a pinch."
  ([pred array]
     (let [acc (transient [])]
       (arrseq [a array]
               (when (pred a)
                 (conj! acc a)))
       (double-array (persistent! acc))))
  ([pred array unit]
     (amap [a array] (if (pred a) a unit))))

(defn afilter2
  "Like afilter, but uses ArrayList instead of transient vectors."
  ([pred array]
     (let [^java.util.ArrayList acc (java.util.ArrayList.)]
       (arrseq [a array]
               (when (pred a)
                 (.add acc a)))
       (double-array acc)))
  ([pred array unit]
     (amap [a array] (if (pred a) a unit))))

(defn aremove
  "Like afilter, but the predicate is reversed."
  [pred & args]
  (apply afilter (complement pred) args))

;; ## Accumulators

(defmacro aproduct
  "Example usage: `(aproduct (double-array [1 2 3]))` => 6.0. See
  abind for information on optional bindings`"
  ([array]
     `(aproduct [a# ~array] a#))
  ([bindings & body]
     `(reduce-with-monoid [unchecked-multiply 1.0]
        ~bindings ~@body)))

(defmacro asum
  "Like aproduct, but uses addition instead of multiplication."
  ([array]
     `(asum [a# ~array] a#))
  ([bindings & body]
     `(reduce-with-monoid [unchecked-add 0.0]
        ~bindings ~@body)))

(defn collect
  "Reduces all xs using `f` and an accumulator set to `unit`. This is
  just a prettier areduce, really."
  [f unit xs]
  (reduce-with-monoid [f unit]
    [x xs] x))