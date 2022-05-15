(ns lisp-in-small-pieces.core)

(def empty-begin
  "An arbitrary value returned by an empty a begin sequence. pg10"
  813)

(declare evaluate)

(defn eprogn
  "Evaluates a sequence form. pg10"
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

(defn wrong [msg & args]
  ;; (throw (Exception. (map pr-str args) msg))
  (throw (Exception. msg)))

(defn lookup
  "pg 13"
  [id env]
  (if (seq? env)
    (let [elem (first env)]
      (if (= (first elem))
        (rest elem)
        (lookup id (rest env))))
    (wrong "No such binding" id)))

(defn extend
  "pg14"
  [env variables values]
  (cond
    (seq? variables) (if (seq? values)
                       (cons (cons (first variables) (first values))
                             (extend env (rest variables) (rest values)))
                       (wrong "Too few values"))
    (empty? variables) (if (empty values)
                         env
                         (wrong "Too many values"))
    (symbol? variables) (cons (cons variables values) env)))

;; (defn evaluate
;;   "The interpreter evaluator. pg7"
;;   [e env]
;;   (if-not (seq? e)
;;     (cond
;;       (symbol? e) (lookup e env)
;;       (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
;;       :else (wrong "Cannot evaluate" e))
;;     (case (first e)
;;       quote (nth e 1)
;;       if (if (evaluate (nth e 1) env)
;;            (evaluate (nth e 2) env)
;;            (evaluate (nth e 3) env))
;;       begin (eprogn (rest e) env)
;;       set! (update (nth e 1) env (evaluate (nth e 2) env))
;;       lambda (make-function (nth e 1) (nth e 2) env)
;;       else (invoke (evaluate (first e) env)
;;                    (evlis (rest e) env)))))
