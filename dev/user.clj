;; Copyright © 2025 Casey Link
;; SPDX-License-Identifier: MIT
(ns user
  (:require
   [starfederation.datastar.clojure.expressions :refer [->expr]]))

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
#_{:clj-kondo/ignore [:type-mismatch]}
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

;; A let block is transpiled to a IIFE, this requires a patched version of datastar
(->expr
 (let [value $my-signal]
   (println value)
   (and (= $my-signal "bear")
        (@post "/foo"))))
;; => "(() => { const value1 = $my-signal; console.log((value1)); return (($my-signal) === (\"bear\")) && (@post(\"/foo\")); })()"

;; JS template strings are supported
;; Since ` is used by the reader, we just wrap the whole thing in quotes
(->expr
 #_{:clj-kondo/ignore [:not-a-function]}
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

;; Known Limitations

;; a generated symbol (el-id below) cannot be used in a template string
(->expr (let [el-id evt.srcElement.id]
          (when el-id
            #_{:clj-kondo/ignore [:not-a-function]}
            (@post ("`/ping/${el-id}`")))))
;; => "(() => { const el_id1 = evt.srcElement.id; if (el_id1) { return @post(`/ping/${el-id}`)} else { return alert(\"No id\")}; })()"

;; No condp
(->expr (condp = $ui._mainMenuOpen
          true (set! $ui._mainMenuOpen false)
          false (set! $ui._mainMenuOpen true)))
;; => "(() => { const pred__288831 = squint_core._EQ_; const expr__288842 = $ui._mainMenuOpen; if (!!(pred__288831(true, expr__288842))) { return $ui._mainMenuOpen = false} else { if (!!(pred__288831(false, expr__288842))) { return $ui._mainMenuOpen = true} else { throw new java.lang.IllegalArgumentException(squint_core.str(\"No matching clause: \", expr__288842))}}; })()"
