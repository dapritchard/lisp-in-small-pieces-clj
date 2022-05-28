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

  (testing "extend-env emtpy env"
    (is (= (extend-env () () ())
           ()))
    (is (= (extend-env () '(a) '(1))
           '((a 1))))
    (is (= (extend-env () '(a bb) '(1 "two"))
           '((a 1)
             (bb "two"))))
    (is (= (extend-env () 'a 1)
           '((a 1)))
        "non-list `variable` and `value`"))

  (testing "extend-env nonempty env"
      (is (= (extend-env '((z 9)) () ())
             '((z 9))))
      (is (= (extend-env '((z 9)) '(a) '(1))
             '((a 1)
               (z 9))))
      (is (= (extend-env '((z 9)) '(z) '(26))
             '((z 26)
               (z 9)))
          "never symbol 'shadows' existing")
      (is (= (extend-env '((z 9)) '(a bb) '(1 "two"))
             '((a 1)
               (bb "two")
               (z 9))))
      (is (= (extend-env '((z 9)) '(a bb z) '(1 "two" 26.0))
             '((a 1)
               (bb "two")
               (z 26.0)
               (z 9))))
      (is (= (extend-env '((z 9) (q "four")) '(a bb) '(1 "two"))
             '((a 1)
               (bb "two")
               (z 9)
               (q "four"))))
      (is (= (extend-env '((z 9)) 'a 1)
             '((a 1)
               (z 9)))
          "non-list `variable` and `value`")))
