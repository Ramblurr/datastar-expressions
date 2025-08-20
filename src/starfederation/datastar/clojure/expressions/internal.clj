;; Copyright © 2025 Casey Link
;; SPDX-License-Identifier: MIT
(ns starfederation.datastar.clojure.expressions.internal
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [clojure.walk :as clojure.walk]
   [squint.compiler :as squint]
   [backtick         :refer [template]]))

(defn bool-expr [e]
  (if (boolean? e)
    e
    (vary-meta e assoc :tag 'boolean)))

(defn expr-js-template [_ _ & args]
  (concat (list 'js* (first args))))

(defn expr-println
  ([_ _ & exprs]
   (let [js (str/join "," (repeat (count exprs) "(~{})"))]
     (concat (list 'js* (str "console.log(" js ")")) exprs))))

(defn expr-do
  ([_ _] nil)
  ([_ _ & exprs]
   (let [js (str/join ", " (repeat (count exprs) "(~{})"))]
     (concat (list 'js* js) exprs))))

(defn expr-if
  [_ _ test & body]
  (list 'if (bool-expr test) (first body) (second body)))

(defn expr-not
  [_ _ x]
  (let [js "(!(~{}))"]
    (bool-expr
     (concat (list 'js* js) (list x)))))

(defn expr-when
  [_ _ test & body]
  (list 'if (bool-expr test) (cons 'expr/do body)))

(defn expr-when-not
  [_ _ test & body]
  (list 'if-not (bool-expr test) (cons 'expr/do body)))

(defn expr-or
  ([_ _] nil)
  ([_ _ x] x)
  ([_ _ x & next]
   (let [js (str/join " || " (repeat (inc (count next)) "(~{})"))]
     (bool-expr
      (concat (list 'js* js) (cons x next))))))

(defn expr-raw
  ([_ _]
   (list 'js* ""))
  ([_ _ x]
   (let [js (str/join (repeat (count x) "~{}"))]
     (concat (list 'js* js) x))))

(defn expr-and
  ([_ _]
   true)
  ([_ _ x]
   x)
  ([_ _ x & next]
   (let [js (str/join " && " (repeat (inc (count next)) "(~{})"))]
     (bool-expr
      (concat (list 'js* js) (cons x next))))))

(defn replace-truth
  "Post-process compiled JavaScript to convert squint_core.truth_(.*) to !!$1"
  [js-string]
  (str/replace js-string #"squint_core\.truth_\((.*)\)" "!!($1)"))

(defn replace-deref
  "Post-processes compiled JavaScript to convert squint_core.deref(fn) to @fn"
  [js-string]
  (-> js-string
      (str/replace #"squint_core\.deref\(squint_core.(get)\)\s*\(" "@$1(")
      (str/replace
       #"squint_core\.deref\(([a-zA-Z_$][a-zA-Z0-9_$]*)\)\s*\("
       "@$1(")))

(defn collect-kebab-signals
  "Collects all kebab-case signal names starting with $ from a form"
  [form]
  (into #{}
        (comp
         (filter symbol?) ;; Keep symbols
         (filter #(and (clojure.string/starts-with? (name %) "$") ;; ...starting with $
                       (clojure.string/includes? (name %) "-"))) ;; ...containing dashes
         (map name))
        (tree-seq coll? seq form)))

(comment
  (= #{"$record-id" "$foo-bar" "$red-panda"}
     (collect-kebab-signals
      `(do (set! ~'$record-id ~collect-kebab-signals)
           (~'@post "/defile-record" {:wut (+ $red-panda ~'$foo-bar)})))))

(defn restore-signals-casing
  "Post-processes compiled js to preserve the original casing of kebab cased signals

  Squint converts symbols like foo-bar into foo_bar, this function reverses that."
  [js-string form]
  (let [kebab-symbols (collect-kebab-signals form)
        replacements  (for [sym  kebab-symbols
                            :let [kebab-case sym
                                  snake-case (clojure.string/replace kebab-case #"-" "_")]]
                        [snake-case kebab-case])]

    (if (empty? replacements)
      js-string
      (reduce (fn [result [from to]]
                (clojure.string/replace result from to))
              js-string
              replacements))))

;; This is how we override squint's special forms (which aren't extensible)
;; We walk the forms before compiling, and replace the special forms with
;; our own macros that compile to the JS that we want.
;; Mainly we want to avoid special forms that rely on the squint core lib
(def macro-replacements {'and      'expr/and
                         'or       'expr/or
                         'if       'expr/if
                         'not      'expr/not
                         'println  'expr/println
                         'when     'expr/when
                         'expr/raw 'expr/raw})

(def compiler-macro-options {'expr {'and         expr-and
                                    'or          expr-or
                                    'when        expr-when
                                    'when-not    expr-when-not
                                    'do          expr-do
                                    'if          expr-if
                                    'not         expr-not
                                    'println     expr-println
                                    'js-template expr-js-template
                                    'raw         expr-raw}})

(defn js* [form]
  (->
   (squint/compile-string (pr-str form)
                          {:elide-imports true
                           :elide-exports true
                           :top-level     false
                           :context       :expr
                           :macros        compiler-macro-options})
   (replace-deref)
   (replace-truth)
   (restore-signals-casing form)
   (str/replace #"\n" " ")
   (str/trim)))

(defn compile [forms]
  (->> forms
       (remove (fn [x] (= x 'do)))
       (map js*)
       (str/join "; ")))

(defn process-string-concat
  "This function converts forms whose head is a symbol starting with $ into string concatenation forms"
  [form]
  (clojure.walk/postwalk
   (fn [node]
     (if (and (sequential? node)
              (symbol? (first node))
              (.startsWith (name (first node)) "$"))
       (let [signal-ref     (name (first node))
             args           (rest node)
             processed-args (map (fn [arg]
                                   (if (and (sequential? arg)
                                            (= 'clojure.core/unquote (first arg)))
                                     (second arg)
                                     arg))
                                 args)]
         (list 'clojure.core/unquote
               (list 'clojure.core/symbol
                     (cons 'str (cons signal-ref processed-args)))))
       node))
   form))

(defn process-not-equals
  "Transforms (not= x y ...) into (not (= x y ...)) to avoid squint_core dependency"
  [form]
  (clojure.walk/postwalk
   (fn [node]
     (if (and (list? node)
              (= 'not= (first node)))
       (list 'not (cons '= (rest node)))
       node))
   form))

(defn process-macros [form]
  (clojure.walk/postwalk
   (fn [node]
     (if (and (list? node)
              (contains? macro-replacements (first node)))
       (cons (get macro-replacements (first node)) (rest node))
       node)) form))

(defn process-interpolation
  "Walks the forms and replaces (\"`foo`\") with (expr/js-template \"foo\")"
  [form]
  (clojure.walk/postwalk
   (fn [node]
     (if (and (list? node)
              (= 1 (count node))
              (string? (first node))
              (= \` (ffirst node)))
       (cons 'expr/js-template node)
       node)) form))

(defprotocol IJSExpression
  (clj [this] "Get clojure form of expression")
  (js  [this] "Get js form of expression"))

(deftype JSExpression [clj-form]
  Object
  (toString [this] (js this))
  IJSExpression
  (clj [_this] clj-form)
  (js  [this] (compile (clj this))))

(defmethod print-method JSExpression [v w]
  (.write w (pr-str (clj v))))

(defmethod print-dup JSExpression [v w]
  (.write w (pr-str (clj v))))

(defn pre-process [forms]
  (-> forms
      process-not-equals
      process-interpolation
      process-string-concat
      process-macros))

(defmacro d*js [& forms]
  (let [processed-form (map pre-process forms)]
    `(->JSExpression (template (do ~@processed-form)))))

(defmacro d*js-str [& forms]
  `(str (d*js ~@forms)))

(comment
  (def record {:record-id 1234})

  (compile `((set! ~'$record-id ~(:record-id record))
             (~'@post "/defile-record")))

  (process-string-concat '(do ($wut "foo")))
  (process-string-concat '(do ($wut "foo" "bar")))
  (let [thing 1234]
    (process-string-concat `(do ($wut ~thing "bar"))))
  ;; => (do (clojure.core/unquote (clojure.core/symbol (str "$wut" 1234 "bar"))))


  ;; basic stuff still works:
  (str (d*js (set! $signal 55))) ;; => "$signal = 55"

  ;; Complex stuff works:
  (def my-d*js (let [x (d*js (.. evt -target -value))
                     num 55
                     y (d*js (+ ~num ~x))]
                 (d*js (set! $signal ~y)
                       (set! $other-signal "no"))))
  (clj my-d*js) ;; => (do (set! $signal (do (+ 55 (do (.. evt -target -value))))) (set! $other-signal "no"))
  (js my-d*js) ;; => "$signal = (55) + (evt.target.value); $other-signal = \"no\""
  (str my-d*js) ;; => "$signal = (55) + (evt.target.value); $other-signal = \"no\""
  (pr-str my-d*js) ;; => "(do (set! $signal (do (+ 55 (do (.. evt -target -value))))) (set! $other-signal \"no\"))"


  ;; Works w/ chassis:
  (require '[dev.onionpancakes.chassis.core :as chassis])
  (let [x (d*js (.. evt -target -value))]
    (chassis/html [:div {:data-on-whatever (d*js (set! $signal ~x))}]))
  ;; => "<div data-on-whatever=\"$signal = evt.target.value\"></div>"


  )
