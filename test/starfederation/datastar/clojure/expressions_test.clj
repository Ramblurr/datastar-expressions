;; Copyright © 2025 Casey Link
;; SPDX-License-Identifier: MIT
(ns starfederation.datastar.clojure.expressions-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [starfederation.datastar.clojure.expressions :refer [->expr]]))

(def record {:record-id "1234"})

(deftest test-basic-assignments
  (testing "basic value assignment with unquote"
    (let [val 42]
      (is (= "$forty-two = 42"
             (->expr (set! $forty-two ~val))))))

  (testing "uuid assignment with stringification"
    (let [val (java.util.UUID/fromString "745a9225-890f-41a7-9fc4-008770a68e7e")]
      (is (= "$forty-two = \"745a9225-890f-41a7-9fc4-008770a68e7e\""
             (->expr (set! $forty-two ~(str val))))))))

(deftest test-case-preservation
  (testing "kebab case preservation"
    (is (= "$record-id = \"1234\""
           (->expr (set! $record-id ~(:record-id record))))))

  (testing "snake case preservation"
    (is (= "$record_id = \"1234\""
           (->expr (set! $record_id ~(:record-id record))))))

  (testing "camelCase preservation"
    (is (= "$recordId = \"1234\""
           (->expr (set! $recordId ~(:record-id record)))))))

(deftest test-namespaced-signals
  (testing "namespaced signal assignment"
    (is (= "$person.first-name = \"alice\""
           (->expr (set! $person.first-name "alice"))))))

(deftest test-arithmetic-operations
  (testing "addition with unquoted value"
    (let [val 1]
      (is (= "$forty-two = (1) + ($forty-one)"
             (->expr (set! $forty-two (+ ~val $forty-one))))))))

(deftest test-function-calls
  (testing "javascript function call"
    (is (= "pokeBear($bear-id)"
           (->expr (pokeBear $bear-id))))))

(deftest test-datastar-actions
  (testing "@get action"
    (is (= "@get(\"/poke\")"
           #_{:clj-kondo/ignore [:type-mismatch]}
           (->expr (@get "/poke")))))

  (testing "@patch action"
    (is (= "@patch(\"/poke\")"
           (->expr (@patch "/poke"))))))

(deftest test-multiple-statements
  (testing "multiple statements in order"
    (is (= "$bear-id = 1234; pokeBear($bear-id); @post(\"/bear-poked\")"
           (->expr
            (set! $bear-id 1234)
            (pokeBear $bear-id)
            (@post "/bear-poked"))))))

(deftest test-dynamic-signal-names
  (testing "dynamic signal name construction"
    (let [field-name "name"]
      (is (= "$bear.name = \"Yogi\"; @post(\"/bear\")"
             (->expr
              (set! ($bear. ~field-name) "Yogi")
              (@post "/bear")))))))

(deftest test-logical-operations
  (testing "logical and"
    (is (= "(($my-signal) === (\"bar\")) && (\"ret-val\")"
           (->expr (and (= $my-signal "bar")
                        "ret-val")))))

  (testing "when expression"
    (is (= "((($my-signal) === (\"bar\")) ? ((\"ret-val\")) : (null))"
           (->expr (when (= $my-signal "bar")
                     "ret-val")))))

  (testing "if expression"
    (is (= "((($my-signal) === (\"bar\")) ? (\"true-val\") : (\"false-val\"))"
           (->expr (if (= $my-signal "bar")
                     "true-val"
                     "false-val")))))

  (testing "complex logical operations"
    (is (= "(((evt.key) === (\"Enter\")) || ((evt.ctrlKey) && ((evt.key) === (\"1\")))) && (alert(\"Key Pressed\"))"
           (->expr (&& (or (= evt.key "Enter")
                           (&& evt.ctrlKey (= evt.key "1")))
                       (alert "Key Pressed")))))))

(deftest test-when-with-multiple-forms
  (testing "when with preventDefault and alert"
    (is (= "(((evt.key) === (\"Enter\")) ? ((evt.preventDefault()), (alert(\"Key Pressed\"))) : (null))"
           (->expr (when (= evt.key "Enter")
                     (evt.preventDefault)
                     (alert "Key Pressed")))))))

(deftest test-data-structures
  (testing "data-class object"
    (is (= "({ \"hidden\": ($fetching-bears) && (($bear-id) === (1)) })"
           (->expr {"hidden" (&& $fetching-bears
                                 (= $bear-id 1))}))))

  (testing "edn to json conversion"
    (is (= "({ \"my-signal\": \"init-value\" })"
           (->expr {:my-signal "init-value"})))))

(deftest test-let-blocks
  (testing "let block with IIFE"
    (is (= "(() => { const value1 = $my-signal; console.log((value1)); return (($my-signal) === (\"bear\")) && (@post(\"/foo\"));  })()"
           (->expr
            (let [value $my-signal]
              (println value)
              (and (= $my-signal "bear")
                   (@post "/foo"))))))))

(deftest test-template-strings
  (testing "javascript template strings"
    (is (= "@post(\"`/ping/${evt.srcElement.id}`\")"
           (->expr
            (@post "`/ping/${evt.srcElement.id}`"))))))

(deftest test-negation
  (testing "simple negation"
    (is (= "(!($foo))"
           (->expr (not $foo)))))

  (testing "negation of equality"
    (is (= "(!((1) === (2)))"
           (->expr (not (= 1 2))))))

  (testing "not equals"
    (is (= "(!(((1) + (3)) === (4)))"
           (->expr (not= (+ 1 3) 4)))))

  (testing "not equals"
    (is (= "(((!(((1) + (3)) === (4)))) ? ((@post(\"/not-equal\"))) : (null))"
           (->expr (when (not= (+ 1 3) 4) (@post "/not-equal"))))))

  (testing "toggle with negation"
    (is (= "$ui._leftnavOpen = (!($ui._leftnavOpen))"
           (->expr (set! $ui._leftnavOpen (not $ui._leftnavOpen)))))))

(deftest test-if-expressions
  (testing "if expression in assignment"
    (is (= "$ui._leftnavOpen = (($ui._leftnavOpen) ? (false) : (true))"
           (->expr (set! $ui._leftnavOpen (if $ui._leftnavOpen false true))))))

  (testing "if with assignments in branches"
    (is (= "(($ui._leftnavOpen) ? ($ui._leftnavOpen = false) : ($ui._leftnavOpen = true))"
           (->expr (if $ui._leftnavOpen
                     (set! $ui._leftnavOpen false)
                     (set! $ui._leftnavOpen true)))))))

(deftest test-expr-raw
  (testing "raw expression with single argument"
    (is (= "$foo = !$foo"
           (->expr (set! $foo (expr/raw "!$foo"))))))

  (testing "raw expression with multiple statements"
    (let [we-are "/back-in-string-concat-land"]
      (is (= "$volume = 11; window.location = /back-in-string-concat-land"
             (->expr
              (set! $volume 11)
              (expr/raw ~(str "window.location = " we-are)))))))

  (testing "raw expression with no arguments"
    (is (= "$foo ="
           (->expr (set! $foo (expr/raw)))))))

(deftest test-bare-symbols
  (testing "bare symbol expression"
    (is (= "$ui._mainMenuOpen"
           (->expr $ui._mainMenuOpen)))))

(deftest test-when-not
  (testing "when-not expression"
    (is (= "(((1) === (1)) ? (null) : ($ui._mainMenuOpen = true))"
           (->expr (when-not (= 1 1)
                     (set! $ui._mainMenuOpen true)))))))

(deftest test-bare-booleans
  (testing "when with bare boolean"
    (is (= "((!!(false)) ? (($foo = true)) : (null))"
           (->expr (when false
                     (set! $foo true)))))))

(deftest test-known-limitations
  (testing "generated symbol in template string"
    (is (= "(() => { const el_id1 = evt.srcElement.id; if (el_id1) { return (@post(\"`/ping/${el-id}`\"))};  })()"
           (->expr (let [el-id evt.srcElement.id]
                     (when el-id
                       (@post "`/ping/${el-id}`"))))))))
