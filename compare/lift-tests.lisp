(defpackage "KMRCL-TESTS-LIFT"
  (:use "COMMON-LISP" "KMRCL" "LIFT"))
(in-package kmrcl-tests-lift)

(deftestsuite test-kmrcl () ())
(deftestsuite test-strings (test-kmrcl) ())
(addtest :str.0
  (ensure-same (substitute-chars-strings "" nil) ""))
(addtest :str.1
  (ensure-same (substitute-chars-strings "abcd" nil) "abcd"))
(addtest :str.2 
  (ensure-same (substitute-chars-strings "abcd" nil) "abcd"))
(addtest :str.3
  (ensure-same (substitute-chars-strings "abcd" '((#\j . "ef"))) "abcd"))
(addtest :str.4
  (ensure-same (substitute-chars-strings "abcd" '((#\a . "ef"))) "efbcd"))
(addtest :str.5
  (ensure-same (substitute-chars-strings "abcd" '((#\a . "ef") (#\j . "ghi")))
               "efbcd"))
(addtest :str.6
  (ensure-same (substitute-chars-strings "abcd" '((#\a . "ef") (#\d . "ghi")))
               "efbcghi"))

(addtest :str.7 
  (ensure-same (escape-xml-string "") ""))
(addtest :str.8 
  (ensure-same (escape-xml-string "abcd") "abcd"))
(addtest :str.9 
  (ensure-same (escape-xml-string "ab&cd") "ab&amp;cd"))
(addtest :str.10 
  (ensure-same (escape-xml-string "ab&cd<") "ab&amp;cd&lt;"))
(addtest :str.12 
  (ensure-same (string-trim-last-character "") ""))
(addtest :str.13 
  (ensure-same (string-trim-last-character "a") ""))
(addtest :str.14 
  (ensure-same (string-trim-last-character "ab") "a"))
(addtest :str.15 
  (ensure-same (nstring-trim-last-character "") ""))
(addtest :str.16 
  (ensure-same (nstring-trim-last-character "a") ""))
(addtest :str.17 
  (ensure-same (nstring-trim-last-character "ab") "a"))

(addtest :str.18
  (ensure-same (delimited-string-to-list "ab|cd|ef" #\|)
               '("ab" "cd" "ef")))
(addtest :str.19 
  (ensure-same (delimited-string-to-list "ab|cd|ef" #\| t)
               '("ab" "cd" "ef")))
(addtest :str.20 
  (ensure-same (delimited-string-to-list "") '("")))
(addtest :str.21 
  (ensure-same (delimited-string-to-list "" #\space t) '("")))
(addtest :str.22 
  (ensure-same (delimited-string-to-list "ab") '("ab")))
(addtest :str.23 
  (ensure-same (delimited-string-to-list "ab" #\space t) '("ab")))
(addtest :str.24 
  (ensure-same (delimited-string-to-list "ab|" #\|) '("ab" "")))
(addtest :str.25 
  (ensure-same (delimited-string-to-list "ab|" #\| t) '("ab")))

(addtest :sdstl.1 
  (ensure-same (string-delimited-string-to-list "ab|cd|ef" "|a")
               '("ab|cd|ef")))
(addtest :sdstl.2 
  (ensure-same (string-delimited-string-to-list "ab|cd|ef" "|")
               '("ab" "cd" "ef")))
(addtest :sdstl.3 
  (ensure-same (string-delimited-string-to-list "ab|cd|ef" "cd")
               '("ab|" "|ef")))
(addtest :sdstl.4
  (ensure-same (string-delimited-string-to-list "ab|cd|ef" "ab")
               '("" "|cd|ef")))
