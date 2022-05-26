(ns lisp-in-small-pieces.core-test
  (:require [clojure.test :refer :all]
            [lisp-in-small-pieces.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

;; (deftest test-lookup
;;   (is (= (lookup 'a ())
;;          ()))
;;   (is (= (lookup 'a ()))))

(deftest test-extend-env
  (testing "extend-env"
    (is (= (extend-env () () ())
           ()))
    (is (= (extend-env () '(a) '(1))
           '((a 1))))
    (is (= (extend-env () '(a bb) '(1 "two"))
           '((a 1)
             (bb "two"))))
    ))
