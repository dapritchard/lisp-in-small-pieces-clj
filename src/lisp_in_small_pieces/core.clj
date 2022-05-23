(ns lisp-in-small-pieces.core
  ;; (:require [lisp-in-small-pieces.primops-construction :refer :all])
  )

(defn wrong
  "Note: not defined in LiSP"
  [msg & args]
  ;; (throw (Exception. (map pr-str args) msg))
  (throw (Exception. msg)))

(def empty-begin
  "An arbitrary value returned by an empty begin sequence
  pg10"
  813)

(declare evaluate)

(defn eprogn
  "Evaluates a sequence form.
  pg10"
  [exps env]
  (if (seq? exps)
    (if (seq? (rest exps))
      (do (evaluate (first exps) env)
          (eprogn (rest exps) env))
      (evaluate (first exps) env))
    empty-begin))

(defn evlis
  "pg12"
  [exps env]
  (if (seq? exps)
    (cons (evaluate (first exps) env)
          (evlis (rest exps) env))
    ()))

(defn lookup
  "pg 13"
  [id env]
  (if (seq? env)
    (let [elem (first env)]
      (if (= (first elem))
        (rest elem)
        (lookup id (rest env))))
    (wrong "No such binding" id)))

(defn extend-env
  "1. Clojure doesn't have the ability to form an improper list, so we can't
  represent all the forms that an abstraction can take in Scheme (as described
  at the bottom of page 14) without creating our own versions of lists. Rather
  than doing this we simply reduce the accepted forms that the variable list
  that function application can take to be a proper list.
  TODO: we could do a symbol here though, right?

  2. Note that this function is called `extend` in LiSP, but here we call it
  `extend-env` to avoid conflict with `clojure.core/extend`.
  pg14"
  [env variables values]
  (if (= (count variables) (count values))
    (into env (zipmap variables values))
    (wrong "The number of variables does not match the number of values")))

(defn invoke
  "pg 15"
  [fn args]
  (if (fn? fn)
    (fn args)
    (wrong "Not a function" fn)))

(defn make-function
  "pg 19"
  [variables body env]
  (fn [values]
    (eprogn body (extend-env env variables values))))

;; (defmacro definitial [name value void]
;;   `(define env-global (en#tuple ['~name ~value]
;;                                 apply env-global)))

;; (defmacro defprimitive (name value arity)
;;   `(definitial ,name
;;      (lambda ($#values)
;;        (if-else (eq ,arity (len $#values))
;;          (,value : :* $#values)
;;          (throw (TypeError (.format "Incorrect arity {}"
;;                                     (en#tuple ,name $#values))))))))

(def env-init ())
(def env-global "pg25" env-init)

;; Lifted from https://github.com/jumarko/lisp-in-small-pieces/blob/c219ff1354366f285000c3efcaabea87cc209a68/clojure/src/ch01_evaluator.clj#L576
(defmacro definitial
  "Defines a new symbol in the global environment bound to given value
  or 'ch01-evaluator/void if no value is provided."
  ([name]
   ;; (prn name) ; the value of name
   ;; (prn (type name)) ; clojure.lang.Symbol
   ;; since definitial is a macro, simply ~name will do the job
   `(definitial ~name :ch01-evaluator/void))
  ([name value]
   ;; notice how we use `extend` instead of relying on internal env structure
   `(alter-var-root #'env-global #(extend-env % ['~name] [~value]))))

;; Lifted from https://github.com/jumarko/lisp-in-small-pieces/blob/c219ff1354366f285000c3efcaabea87cc209a68/clojure/src/ch01_evaluator.clj#L607
(defmacro defprimitive
  "Defines a primitive operation denoted by the symbol with given name,
  implemented as function f of given arity."
  [name f arity]
  `(definitial
     ~name
     (fn [~'values]
       (if (= ~arity (count ~'values))
         (apply ~f ~'values)
         (wrong "Incorrect ~arity" [~f ~'values])))))


(definitial t true)
(definitial f false)
(definitial nil nil)

;; Add a few functions to the global environment as examples. Note that in LiSP
;; the function `set-cdr!` is also defined, but since this requires a little bit
;; more effort in Clojure we skip this function
;; pg 27
(defprimitive cons cons 2)
(defprimitive car first 1)
(defprimitive + + 2)
(defprimitive eq? = 2)
(defprimitive < < 2)

(defn evaluate
  "The interpreter evaluator. pg7"
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (nth e 1)
      if (if (evaluate (nth e 1) env)
           (evaluate (nth e 2) env)
           (evaluate (nth e 3) env))
      begin (eprogn (rest e) env)
      set! (update (nth e 1) env (evaluate (nth e 2) env))
      lambda (make-function (nth e 1) (nth e 2) env)
      else (invoke (evaluate (first e) env)
                   (evlis (rest e) env)))))
