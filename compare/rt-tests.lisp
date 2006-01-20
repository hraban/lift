#|
non-interactive
can you change the test
|#

(in-package #:cl)
(defpackage #:kmrcl-tests-rt
  (:use #:kmrcl #:cl #:rtest))
(in-package #:kmrcl-tests-rt)

(rem-all-tests)


(deftest :str.0 (substitute-chars-strings "" nil) "")
(deftest :str.1 (substitute-chars-strings "abcd" nil) "abcd")
(deftest :str.2 (substitute-chars-strings "abcd" nil) "abcd")
(deftest :str.3 (substitute-chars-strings "abcd" '((#\j . "ef"))) "abcd")
(deftest :str.4 (substitute-chars-strings "abcd" '((#\a . "ef"))) "efbcd")
(deftest :str.5
    (substitute-chars-strings "abcd" '((#\a . "ef") (#\j . "ghi")))
  "efbcd")
(deftest :str.6
    (substitute-chars-strings "abcd" '((#\a . "ef") (#\d . "ghi")))
  "efbcghi")

(deftest :str.7 (escape-xml-string "") "")
(deftest :str.8 (escape-xml-string "abcd") "abcd")
(deftest :str.9 (escape-xml-string "ab&cd") "ab&amp;cd")
(deftest :str.10 (escape-xml-string "ab&cd<") "ab&amp;cd&lt;")
(deftest :str.12 (string-trim-last-character "") "")
(deftest :str.13 (string-trim-last-character "a") "")
(deftest :str.14 (string-trim-last-character "ab") "a")
(deftest :str.15 (nstring-trim-last-character "") "")
(deftest :str.16 (nstring-trim-last-character "a") "")
(deftest :str.17 (nstring-trim-last-character "ab") "a")

(deftest :str.18 (delimited-string-to-list "ab|cd|ef" #\|)
					  ("ab" "cd" "ef"))
(deftest :str.19 (delimited-string-to-list "ab|cd|ef" #\| t)
					  ("ab" "cd" "ef"))
(deftest :str.20 (delimited-string-to-list "") (""))
(deftest :str.21 (delimited-string-to-list "" #\space t) (""))
(deftest :str.22 (delimited-string-to-list "ab") ("ab"))
(deftest :str.23 (delimited-string-to-list "ab" #\space t) ("ab"))
(deftest :str.24 (delimited-string-to-list "ab|" #\|) ("ab" ""))
(deftest :str.25 (delimited-string-to-list "ab|" #\| t) ("ab"))

(deftest :sdstl.1 (string-delimited-string-to-list "ab|cd|ef" "|a")
  ("ab|cd|ef"))
(deftest :sdstl.2 (string-delimited-string-to-list "ab|cd|ef" "|")
  ("ab" "cd" "ef"))
(deftest :sdstl.3 (string-delimited-string-to-list "ab|cd|ef" "cd")
  ("ab|" "|ef"))
(deftest :sdstl.4 (string-delimited-string-to-list "ab|cd|ef" "ab")
  ("" "|cd|ef"))

