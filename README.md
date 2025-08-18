# `expressions`

> Clojure to Datastar expression transpiler

`expressions` is a proof-of-concept for writing [🚀 datastar][dt] expressions using Clojure without manual string concatenation.

Instead of:

```clojure
[:button {:data-on-click (str "$person-id" (:id person) " @post('/update-person')")}]`
```


Write this:

```clojure
[:button {:data-on-click (->expr
                          (set! $person-id ~(:id person))
                          (@post "/update-person"))}]
```

It is powered by [squint](https://github.com/squint-cljs/squint), thanks [@borkdude][bork].

## Goal & Non-Goals

Since Clojure does not have string interpolation, writing even simple [Datastar
(d\*) expressions][dt-expr] can involve a lot of `str` or `format` gymnastics.

The goal of `expressions` is to add a little bit of syntax sugar when writing
d\* expressions so that they can be read and manipulated as s-expressions.

D\* expressions are not exactly javascript, though they are interpreted by the
js runtime. D\* expressions also do not have a grammar or really any formal
definition of any kind. Delaney's official position is that the simplest
and obvious expressions a human would write should work.

`expressions` follows that by trying to provide coverage for 99% of simple and
obvious expressions.

⚠️ You can totally write expressions that result in broken javascript, that is not
necessarily a bug.

## Install

```clojure
datastar/expressions {:git/url "https://github.com/outskirtslabs/datastar-expressions/"
                      :git/sha "53efc7093be1ba33b331b4a27884be8925d8bdce"}
```

## Status

`expressions` is experimental and breaking changes will occur as it is actively being developed. Please share your feedback so we can squash bugs and arrive at a stable release.

## REPL Exploration

To see what this is all about, you can clone this repo and play with the demos:

```
clojure -M:dev ;; (bring your own repl server)
```

Check out [`dev/user.clj`](./dev/user.clj) and [`dev/demo.clj`](./dev/demo.clj)


## Example Usage

```clojure
(ns user
  (:require [starfederation.datastar.clojure.expressions :refer [->expr]]))

;; Samples

(def record {:record-id "1234"})

;; You have to unquote (~) forms you want evaluated
;; Otherwise no quoting is needed!
;; vars and locals are available for evaluation
(let [val 42]
  (->expr
   (set! $forty-two ~val)))
;; => "$forty-two = 42;"

(let [val (random-uuid)]
  (->expr
   (set! $forty-two ~(str val))))
;; => "$forty-two = \"745a9225-890f-41a7-9fc4-008770a68e7e\";"

;; kebab case preservation
(->expr
 (set! $record-id ~(:record-id record)))
;; => "$record-id = \"1234\";"

;; actually... all case preservation :)
(->expr
 (set! $record_id ~(:record-id record)))
;; => "$record_id = \"1234\";"

(->expr
 (set! $recordId ~(:record-id record)))
;; => "$recordId = \"1234\";"

;; namespaced signals work of course
(->expr
 (set! $person.first-name "alice"))
;; => "$person.first-name = \"alice\";"

;; primitive functions work too (squint adds parens, but its ok)
(let [val 1]
  (->expr
   (set! $forty-two (+ ~val $forty-one))))
;; => "$forty-two = (1) + ($forty-one);"

;; calling js functions:
(->expr (pokeBear $bear-id))
;; => "pokeBear($bear-id)"

;; actions
(->expr (@get "/poke"))
;; => "@get(\"/poke\")"

(->expr (@patch "/poke"))
;; => "@patch(\"/poke\")"

;; expr with multiple statements are in order like you would expect
(->expr
 (set! $bear-id 1234)
 (pokeBear $bear-id)
 (@post "/bear-poked"))
;; => "$bear-id = 1234;; pokeBear($bear-id); @post(\"/bear-poked\")"

;; You can build dynamic signal names by using the $signal in the first position
(let [field-name "name"]
  (->expr
   (set! ($bear. ~field-name) "Yogi")
   (@post "/bear")))
;; => "$bear.name = \"Yogi\";; @post(\"/bear\")"

;; logical conjunctions and disjunctions
(->expr (and (= $my-signal "bar")
             "ret-val"))
;; => "(($my-signal) === (\"bar\")) && (\"ret-val\")"

;; But you should probably use when/if
(->expr (when (= $my-signal "bar")
          "ret-val"))
;; => "((($my-signal) === (\"bar\")) ? ((\"ret-val\")) : (null))"
(->expr (if (= $my-signal "bar")
          "true-val"
          "false-val"))
;; => "((($my-signal) === (\"bar\")) ? (\"true-val\") : (\"false-val\"))"

;; A few other variations
(->expr (&& (or (= evt.key "Enter")
                (&& evt.ctrlKey (= evt.key "1")))
            (alert "Key Pressed")))
;; => "(((evt.key) === (\"Enter\")) || ((evt.ctrlKey) && ((evt.key) === (\"1\")))) && (alert(\"Key Pressed\"))"

;; This one is interesting, see how it uses the , operator to separate sub-expressions
(->expr (when  (= evt.key "Enter")
          (evt.preventDefault)
          (alert "Key Pressed")))
;; => "(((evt.key) === (\"Enter\")) ? ((evt.preventDefault()), (alert(\"Key Pressed\"))) : (null))"

;; And here is one for data-class
(->expr {"hidden" (&& $fetching-bears
                      (= $bear-id 1))})
;; => "({ \"hidden\": ($fetching-bears) && (($bear-id) === (1)) })"

;; It also does edn->json conversion, so setting initial signals is possible
(->expr {:my-signal "init-value"})
;; => "({ \"my-signal\": \"init-value\" })"

(->expr
 (let [value $my-signal]
   (println value)
   (and (= $my-signal "bear")
        (@post "/foo"))))
;; => "(() => { const value1 = $my-signal; console.log((value1)); return (($my-signal) === (\"bear\")) && (@post(\"/foo\")); })()"

;; JS template strings are supported
;; Since ` is used by the reader, we just wrap the whole thing in quotes
(->expr
 (@post ("`/ping/${evt.srcElement.id}`")))
;; => "@post(`/ping/${evt.srcElement.id}`)"

;; Negation
(->expr (not $foo))
;; => "(!($foo))"
(->expr (not (= 1 2)))
;; => "(!((1) === (2)))"
(->expr (not= (+ 1 3)  4))
;; => "((1) + (3)) !== (4)"
(->expr (set! $ui._leftnavOpen (not $ui._leftnavOpen)))
;; => "$ui._leftnavOpen = (!($ui._leftnavOpen))"

;; if
(->expr (set! $ui._leftnavOpen (if $ui._leftnavOpen false true)))
;; => "$ui._leftnavOpen = (($ui._leftnavOpen) ? (false) : (true))"

(->expr (if $ui._leftnavOpen
          (set! $ui._leftnavOpen false)
          (set! $ui._leftnavOpen true)))
;; => "(($ui._leftnavOpen) ? ($ui._leftnavOpen = false) : ($ui._leftnavOpen = true))"

;; expr/raw is an escape hatch to emit raw JS
;; raw/1 emits its argument as is
(->expr (set! $foo (expr/raw "!$foo")))
;; => "$foo = !$foo"

(let [we-are "/back-in-string-concat-land"]
  (->expr
   (set! $volume 11)
   (expr/raw ~(str "window.location = " we-are))))
;; => "$volume = 11; window.location = /back-in-string-concat-land"

;; raw/0 emits nothing
(->expr (set! $foo (expr/raw)))
;; => "$foo ="

;; bare symbols
(->expr $ui._mainMenuOpen)
;; => "$ui._mainMenuOpen"

;; when-not
(->expr (when-not (= 1 1)
          (set! $ui._mainMenuOpen true)))
;; => "(((1) === (1)) ? (null) : ($ui._mainMenuOpen = true))"

;; bare booleans
(->expr (when false
          (set! $foo true)))
;; => "((!!(false)) ? (($foo = true)) : (null))"
```

## Known Limitations

``` clojure
;; a generated symbol (el-id below) cannot be used in a template string
(->expr (let [el-id evt.srcElement.id]
          (when el-id
            (@post ("`/ping/${el-id}`")))))
;; => "(() => { const el_id1 = evt.srcElement.id; if (el_id1) { return @post(`/ping/${el-id}`)} else { return alert(\"No id\")}; })()"

;; No condp
(->expr (condp = $ui._mainMenuOpen
          true (set! $ui._mainMenuOpen false)
          false (set! $ui._mainMenuOpen true)))
;; => "(() => { const pred__288831 = squint_core._EQ_; const expr__288842 = $ui._mainMenuOpen; if (!!(pred__288831(true, expr__288842))) { return $ui._mainMenuOpen = false} else { if (!!(pred__288831(false, expr__288842))) { return $ui._mainMenuOpen = true} else { throw new java.lang.IllegalArgumentException(squint_core.str(\"No matching clause: \", expr__288842))}}; })()"
```

## License: MIT License

Copyright © 2025 Casey Link <casey@outskirtslabs.com>

Distributed under the [MIT](https://spdx.org/licenses/MIT.html).


[bork]: https://github.com/borkdude
[dt]: https://data-star.dev
[dt-dev]: https://github.com/starfederation/datastar/tree/develop
[dt-expr]: https://data-star.dev/guide/datastar_expressions


