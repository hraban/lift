#|
How can you run all of the tests in a hierarchy?
Verbose

Times the tests
|#

(in-package #:cl)
(defpackage #:kmrcl-tests-xlunit
  (:use #:kmrcl #:cl #:xlunit))
(in-package #:kmrcl-tests-xlunit)

(defclass string-test-case (test-case)
  ())
(def-test-method :str.0 ((test string-test-case) :run nil)
  (assert-true (string-equal (substitute-chars-strings "" nil) "")))
(def-test-method  :str.1 ((test string-test-case) :run nil)
  (assert-true (string-equal (substitute-chars-strings "abcd" nil) "abcd")))


(defclass string-delimited-string-to-list-test-case (string-test-case)
  ())
(def-test-method :str.0 ((test string-delimited-string-to-list-test-case) :run nil)
  (assert-true (equal
                (string-delimited-string-to-list "ab|cd|ef" "|a")
                '("ab|cd|ef"))))

(textui-test-run (get-suite string-test-case))
(textui-test-run (get-suite string-delimited-string-to-list-test-case))
