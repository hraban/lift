;;;-*- Mode: Lisp; Package: LIFT -*-

#| simple-header

Copyright (c) 2001-2006 Gary Warren King (gwking@cs.umass.edu) 

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions: 

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE. 

|#
(in-package #:lift)

(deftestsuite test-lift () ())

;;; ---------------------------------------------------------------------------
;;; test-lift-ensure
;;; make sure that ensure and its friends work as expected
;;;
;;; The strategy here is to pair "regular" tests with meta-tests. The 
;;; regular tests are normal tests written using LIFT. The meta-tests
;;; use run-tests or run-tests to run the regular test and then grovel
;;; over the returned test-result to make sure it contains what it is
;;; supposed to.
;;; ---------------------------------------------------------------------------

(deftestsuite test-lift-ensure (test-lift) ())
(deftestsuite test-lift-ensure-helper () ())

(addtest (test-lift-ensure-helper)
  simple-ensure-test-1
  (ensure t))

(addtest (test-lift-ensure)
  simple-ensure-test-1
  (let ((tr (run-test :suite 'test-lift-ensure-helper :name 'simple-ensure-test-1)))
    (ensure-same (length (tests-run tr)) 1)
    (ensure-same (failures tr) nil)
    (ensure-same (errors tr) nil)
    (ensure-same (test-mode tr) :single)
    (ensure-same (test-interactive? tr) nil)
    (ensure-same (tests-run tr) '("SIMPLE-ENSURE-TEST-1"))))

;;; ---------------------------------------------------------------------------

(addtest (test-lift-ensure-helper)
  simple-ensure-test-2
  (ensure nil))

(addtest (test-lift-ensure)
  simple-ensure-test-2
  (let ((tr (run-test :suite 'test-lift-ensure-helper :name 'simple-ensure-test-2)))
    (ensure-same (length (tests-run tr)) 1 :report "Number of tests-run")
    (ensure-same (length (failures tr)) 1 :report "Number of failures")
    (ensure-same (errors tr) nil :report "Number of errors")
    (ensure-same (tests-run tr) '("SIMPLE-ENSURE-TEST-2"))))

;;; ---------------------------------------------------------------------------

(addtest (test-lift-ensure-helper)
  simple-ensure-test-3
  (ensure (let ((x 0)) (/ x))))

(addtest (test-lift-ensure)
  simple-ensure-test-3
  (let ((tr (run-test :suite 'test-lift-ensure-helper :name 'simple-ensure-test-3)))
    (ensure-same (length (tests-run tr)) 1)
    (ensure-same (length (failures tr)) 0)
    (ensure-same (length (errors tr)) 1)
    (ensure-same (tests-run tr) '("SIMPLE-ENSURE-TEST-3"))))


;;; ---------------------------------------------------------------------------
;;; test-lift-setup-teardown
;;; make sure that setup and teardown happen in the right order
;;; ---------------------------------------------------------------------------

(deftestsuite test-lift-setup-teardown (test-lift) ())

(deftestsuite test-lift-setup-teardown-1 (test-lift-setup-teardown) ()
  (:setup (push 1 *test-scratchpad*))
  (:teardown (push :a *test-scratchpad*))
  (:tests (setup-teardown-1 (push 'test-1 *test-scratchpad*))))

(addtest (test-lift-setup-teardown)
  setup-teardown-1
  (setf *test-scratchpad* nil)
  (run-test
   :name 'setup-teardown-1
   :suite 'test-lift-setup-teardown-1
   :result (make-test-result 'test-lift-setup-teardown-1 :single))
  (ensure-same (reverse *test-scratchpad*)
               '(1 test-1 :a)))

(addtest (test-lift-setup-teardown) 
  setup-teardown-1-all
  (setf *test-scratchpad* nil)
  (run-tests 
   :suite 'test-lift-setup-teardown-1
   :result (make-test-result 'test-lift-setup-teardown-1 :multiple))
  (ensure-same (reverse *test-scratchpad*)
               '(1 test-1 :a 1 2 test-2 :b :a 1 2 3 test-3 :c :b :a)))

;;; ---------------------------------------------------------------------------

(deftestsuite test-lift-setup-teardown-2 (test-lift-setup-teardown-1) ()
  (:setup (push 2 *test-scratchpad*))
  (:teardown (push :b *test-scratchpad*))
  (:tests (setup-teardown-2 (push 'test-2 *test-scratchpad*))))

(deftestsuite test-lift-setup-teardown-3 (test-lift-setup-teardown-2) ()
  (:setup (push 3 *test-scratchpad*))
  (:teardown (push :c *test-scratchpad*))
  (:tests (setup-teardown-3 (push 'test-3 *test-scratchpad*))))

(addtest (test-lift-setup-teardown) 
  setup-teardown-3
  (setf *test-scratchpad* nil)
  (run-test
   :name setup-teardown-3
   :suite 'test-lift-setup-teardown-3
   :result (make-test-result 'test-lift-setup-teardown-3 :single))
  (ensure-same (reverse *test-scratchpad*)
               '(1 2 3 test-3 :c :b :a)))

(addtest (test-lift-setup-teardown)
  setup-teardown-3-all
  (setf *test-scratchpad* nil)
  (run-tests 
   :suite 'test-lift-setup-teardown-3
   :result (make-test-result 'test-lift-setup-teardown-3 :multiple))
  (ensure-same (reverse *test-scratchpad*)
               '(1 2 3 test-3 :c :b :a)))

;;; ---------------------------------------------------------------------------
;;; test ensure same
;;; ---------------------------------------------------------------------------

(deftestsuite test-lift-ensure-same (test-lift)
  ())

;;; ---------------------------------------------------------------------------

;;?? Gary King 2004-06-21: not really a test yet, more of a syntax works check
(addtest (test-lift-ensure-same)
  (ensure-same 2 2 :test =)
  (ensure-same 2 2 :test '=)
  (ensure-same 2 2 :test #'=))

;;; ---------------------------------------------------------------------------
;;; test single setup
;;; ---------------------------------------------------------------------------

(deftestsuite test-single-setup (test-lift) ())

;; helpers
(deftestsuite test-single-setup-helper () ())
(deftestsuite test-single-setup-child-a (test-single-setup-helper) () 
  (:setup (push :a *test-scratchpad*))
  (:test (test-1 (ensure t))))
(deftestsuite test-single-setup-child-a-1 (test-single-setup-child-a) () 
  (:setup (push :a-1 *test-scratchpad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b (test-single-setup-helper) ()
  (:setup (push :b *test-scratchpad*))
  (:test (test-1 (ensure t))))
(deftestsuite test-single-setup-child-b-1-ss (test-single-setup-child-b) ()
  (:single-setup)
  (:setup (push :b-1 *test-scratchpad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))
(deftestsuite test-single-setup-child-b-1-a (test-single-setup-child-b-1-ss) ()
  (:setup (push :b-1-a *test-scratchpad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))
(deftestsuite test-single-setup-child-b-1-b (test-single-setup-child-b-1-ss) ()
  (:setup (push :b-1-b *test-scratchpad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-c (test-single-setup-helper) ()
  (:setup (push :c *test-scratchpad*))
  (:test (test-1 (ensure t))))
(deftestsuite test-single-setup-child-c-1 (test-single-setup-child-c) ()
  (:setup (push :c-1 *test-scratchpad*))
  (:test (test-1 (ensure t))))

;;; ---------------------------------------------------------------------------

(addtest (test-single-setup)
  test-a-multiple-setup
  (setf *test-scratchpad* nil)
  (run-test :suite 'test-single-setup-child-a-1 :name 'test-1)
  (run-test :suite 'test-single-setup-child-a-1 :name 'test-2)
  (ensure-same *test-scratchpad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-b-single-setup-1
  (setf *test-scratchpad* nil)
  (run-test :suite 'test-single-setup-child-b-1-ss :name 'test-1)
  (run-test :suite 'test-single-setup-child-b-1-ss :name 'test-2)
  
  ;; single tests do all the setup so this should be exactly the same
  (ensure-same *test-scratchpad* '(:b-1 :b :b-1 :b)))

(addtest (test-single-setup)
  test-b-single-setup-2
  (setf *test-scratchpad* nil)
  (run-tests :suite 'test-single-setup-child-b-1-ss :do-children? nil)
  (ensure-same *test-scratchpad* '(:b-1 :b)))

;;; ---------------------------------------------------------------------------
;;; warning behavior
;;; ---------------------------------------------------------------------------

(deftestsuite test-ignore-warnings (test-lift) ()
  (:setup (setf *test-scratchpad* nil)))

(deftestsuite test-ignore-warnings-helper () ())

(deftestsuite test-ignore-warnings-helper-warning (test-ignore-warnings-helper) ()
  (:test (do-it 
          (push :a *test-scratchpad*)
          (warn "Ouch")
          (push :b *test-scratchpad*))))

(deftestsuite test-ignore-warnings-helper-no-warning (test-ignore-warnings-helper) ()
  (:test (do-it
          (push :a *test-scratchpad*)
          (+ 2 2)
          (push :b *test-scratchpad*))))

(addtest (test-ignore-warnings)
  test-has-warning
  (run-test :suite 'test-ignore-warnings-helper-warning :name 'do-it)
  (ensure-same *test-scratchpad* '(:b :a)))

(addtest (test-ignore-warnings)
  test-has-no-warning
  (run-test :suite 'test-ignore-warnings-helper-no-warning :name 'do-it)
  (ensure-same *test-scratchpad* '(:b :a)))

;;; ---------------------------------------------------------------------------
;;; test-environment stays clean
;;; ---------------------------------------------------------------------------

(deftestsuite test-lift-environment-pristine (test-lift) ()
  (:setup (setf *test-environment* nil)))
(deftestsuite test-lift-environment-pristine-helper ()
  ((a 2)
   (b (* a a))))

(addtest (test-lift-environment-pristine-helper)
  do-it
  (ensure-same (* a a) b))

(addtest (test-lift-environment-pristine)
  test-1
  (run-test :suite 'test-lift-environment-pristine-helper :name 'do-it)
  (ensure (null *test-environment*)))


;;; ---------------------------------------------------------------------------
;;; test-creating-multiple-tests
;;; ---------------------------------------------------------------------------

(deftestsuite test-creating-multiple-tests (test-lift)
  ())

(deftestsuite test-creating-multiple-tests-helper ()
 ()
 (:tests ((ensure-same 1 1)
          (ensure-same 2 2))
         ((ensure-same 3 3))))

(addtest (test-creating-multiple-tests)
  test-1
  (ensure-same (testsuite-test-count 'test-creating-multiple-tests-helper) 2))

