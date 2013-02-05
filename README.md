# array-utils

`array-utils` is a small lib inspired by Prismatic's Flop. It offers
more elegant ways to handle primitive arrays of doubles. Includes
functional programmingstaples like `afilter` and `amap`, as well as
in-place versions like `afill!` and `afilter!`. The functions require
little or no manual type hinting, and most of them support binding
semantics similar to for-each in imperative languages. (See `abind`).

**NB.** Please consider using built-in Clojure functions and data
structures unless you need the raw speed of Java primitives. Please
think of the children.

## Usage

Pull the repo, run `lein deps`, and require `array-utils.core`. 

## Motivation

Instead of 

```clojure
(defn dot-product [^doubles ws ^doubles xs]
  (areduce xs i ret 0.0
    (+ ret (* (aget xs i)
              (aget ws i))))
```

you can write

```clojure
(defn dot-product [ws xs] (asum [x xs w ws] (* x w)))
```

See `src/array-utils/example.clj` for more.

## Documentation

See the docstrings. Generate the HTML documentation by running `lein marg`.

## License

Copyright © 2013 Emil Flakk

Distributed under the Eclipse Public License, the same as Clojure.

