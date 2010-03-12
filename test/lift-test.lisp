;;;-*- Mode: Lisp; Package: LIFT -*-

#| 

See file COPYING for license

|#

(in-package #:lift-test)

(deftestsuite lift-test () 
  ()
  (:dynamic-variables
   (*test-break-on-errors?* nil)
   (*test-break-on-failures?* nil)))

;;; ---------------------------------------------------------------------------
;;; lift-test-ensure
;;; make sure that ensure and its friends work as expected
;;;
;;; The strategy here is to pair "regular" tests with meta-tests. The 
;;; regular tests are normal tests written using LIFT. The meta-tests
;;; use run-tests or run-tests to run the regular test and then grovel
;;; over the returned test-result to make sure it contains what it is
;;; supposed to.
;;;
;;; Note that if we don't pass in :report-pathname nil, then we'll get a lot
;;; of spurious extra report files...
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-ensure (lift-test) ())

(deftestsuite lift-test-ensure-helper () ())

(addtest (lift-test-ensure-helper)
  simple-ensure-test-1
  (ensure t))

(addtest (lift-test-ensure)
  simple-ensure-test-1
  (let ((tr (run-test :suite 'lift-test-ensure-helper
		      :name 'simple-ensure-test-1)))
    (ensure-same (length (tests-run tr)) 1)
    (ensure-null (failures tr))
    (ensure-null (errors tr))
    (ensure-same (test-mode tr) :single)
    (ensure-same (mapcar #'second (tests-run tr)) 
		 '(lift-test::simple-ensure-test-1))))

;;; ---------------------------------------------------------------------------

(addtest (lift-test-ensure-helper)
  simple-ensure-test-2
  (ensure nil))

(addtest (lift-test-ensure)
  simple-ensure-test-2
  (let ((tr (run-test :suite 'lift-test-ensure-helper
		      :name 'simple-ensure-test-2)))
    (ensure-same (length (tests-run tr)) 1 :report "Number of tests-run")
    (ensure-same (length (failures tr)) 1 :report "Number of failures")
    (ensure-null (errors tr) :report "Number of errors")
    (ensure-same (mapcar #'second (tests-run tr))
		 '(lift-test::simple-ensure-test-2))))

;;; ---------------------------------------------------------------------------

(addtest (lift-test-ensure-helper)
  simple-ensure-test-3
  (handler-case 
      (ensure (let ((x 0)) (/ x)))
    (error (c)
      (print c)
      (error c))))

(addtest (lift-test-ensure)
  simple-ensure-test-3
  (let ((tr (run-test :suite 'lift-test-ensure-helper
		      :name 'simple-ensure-test-3)))
    (ensure-same (length (tests-run tr)) 1)
    (ensure-same (length (failures tr)) 0)
    (ensure-same (length (errors tr)) 1)
    (ensure-same (mapcar #'second (tests-run tr)) 
		 '(lift-test::simple-ensure-test-3))))


;;; ---------------------------------------------------------------------------
;;; lift-test-setup-teardown
;;; make sure that setup and teardown happen in the right order
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-setup-teardown (lift-test) ())

(deftestsuite lift-test-setup-teardown-1 (lift-test-setup-teardown) ()
  (:setup (push 1 *test-notepad*))
  (:teardown (push :a *test-notepad*))
  (:tests (setup-teardown-1 (push 'test-1 *test-notepad*))))

(addtest (lift-test-setup-teardown)
  setup-teardown-1
  (setf *test-notepad* nil)
  (run-test
   :name 'setup-teardown-1
   :suite 'lift-test-setup-teardown-1
   :result (make-test-result 'lift-test-setup-teardown-1 :single))
  (ensure-same (reverse *test-notepad*)
               '(1 test-1 :a)))

(addtest (lift-test-setup-teardown) 
  setup-teardown-1-all
  (setf *test-notepad* nil)
  (run-tests 
   :suite 'lift-test-setup-teardown-1
   :result (make-test-result 'lift-test-setup-teardown-1 :multiple)
   :report-pathname nil)
  (ensure-same (reverse *test-notepad*)
               '(1 test-1 :a 1 2 test-2 :b :a 1 2 3 test-3 :c :b :a)))

(deftestsuite lift-test-setup-teardown-2 (lift-test-setup-teardown-1) ()
  (:setup (push 2 *test-notepad*))
  (:teardown (push :b *test-notepad*))
  (:tests (setup-teardown-2 (push 'test-2 *test-notepad*))))

(deftestsuite lift-test-setup-teardown-3 (lift-test-setup-teardown-2) ()
  (:setup (push 3 *test-notepad*))
  (:teardown (push :c *test-notepad*))
  (:tests (setup-teardown-3 (push 'test-3 *test-notepad*))))

(addtest (lift-test-setup-teardown) 
  setup-teardown-3
  (setf *test-notepad* nil)
  (run-test
   :name 'setup-teardown-3
   :suite 'lift-test-setup-teardown-3
   :result (make-test-result 'lift-test-setup-teardown-3 :single))
  (ensure-same (reverse *test-notepad*)
               '(1 2 3 test-3 :c :b :a)))

(addtest (lift-test-setup-teardown)
  setup-teardown-3-all
  (setf *test-notepad* nil)
  (run-tests 
   :suite 'lift-test-setup-teardown-3
   :result (make-test-result 'lift-test-setup-teardown-3 :multiple)
   :report-pathname nil)
  (ensure-same (reverse *test-notepad*)
               '(1 2 3 test-3 :c :b :a)))

;;; ---------------------------------------------------------------------------
;;; test ensure same
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-ensure-comparisons (lift-test)
  ())

;;?? Gary King 2004-06-21: not really a test yet, more of a syntax works check
(addtest (lift-test-ensure-comparisons)
  same-basic
  (ensure-same 2 2)
  (ensure-same 2 2 :test =)
  (ensure-same 2 2 :test '=)
  (ensure-same 2 2 :test #'=))

(addtest (lift-test-ensure-comparisons)
  same-test-flet
  (flet ((check (a b)
	   (= (abs a) (abs b))))
    (ensure-same 2 -2 :test #'check)
    (ensure-same 2 -2 :test 'check)
    (ensure-same 2 -2 :test check)))

(addtest (lift-test-ensure-comparisons)
  same-test-labels
  (labels ((check (a b)
	     (= (abs a) (abs b))))
    (ensure-same 2 -2 :test #'check)
    (ensure-same 2 -2 :test 'check)
    (ensure-same 2 -2 :test check)))

(defun %make-test-ensure-same-test (fn)
  (lambda (a b)
    (funcall fn a b)))

(addtest (lift-test-ensure-comparisons)
  same-test-with-test-maker
  (ensure-same 2 2 :test (%make-test-ensure-same-test #'=)))

;;?? Gary King 2004-06-21: not really a test yet, more of a syntax works check
(addtest (lift-test-ensure-comparisons)
  different-basic
  (ensure-different 2 -12)
  (ensure-different -2 2 :test =)
  (ensure-different 20 2 :test '=)
  (ensure-different 2 2.1 :test #'=))

(addtest (lift-test-ensure-comparisons)
  different-test-flet
  (flet ((check (a b)
	   (= (abs a) (abs b))))
    (ensure-different 2 -2.1 :test #'check)
    (ensure-different 1.9 -2 :test 'check)
    (ensure-different 20 -2 :test check)))

(addtest (lift-test-ensure-comparisons)
  different-test-labels
  (labels ((check (a b)
	     (= (abs a) (abs b))))
    (ensure-different 2 -2.1 :test #'check)
    (ensure-different 1.9 -2 :test 'check)
    (ensure-different 20 -2 :test check)))

(addtest (lift-test-ensure-comparisons)
  different-test-with-test-maker
  (ensure-different 20 2 :test (%make-test-ensure-same-test #'=)))



;;; ---------------------------------------------------------------------------
;;; test single setup
;;; ---------------------------------------------------------------------------

(deftestsuite test-single-setup (lift-test) ())

;; helpers
(deftestsuite test-single-setup-helper () ())

(deftestsuite test-single-setup-child-a (test-single-setup-helper) () 
  (:setup (push :a *test-notepad*))
  (:test (test-1 (ensure t))))

(deftestsuite test-single-setup-child-a-1 (test-single-setup-child-a) () 
  (:setup (push :a-1 *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b (test-single-setup-helper) ()
  (:setup (push :b *test-notepad*))
  (:test (test-1 (ensure t))))

(deftestsuite test-single-setup-child-b-1-ss (test-single-setup-child-b) ()
  (:run-setup :once-per-suite)
  (:setup (push :b-1 *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b-1-a (test-single-setup-child-b-1-ss) ()
  (:setup (push :b-1-a *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b-1-b (test-single-setup-child-b-1-ss) ()
  (:setup (push :b-1-b *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-c (test-single-setup-helper) ()
  (:setup (push :c *test-notepad*))
  (:test (test-1 (ensure t))))

(deftestsuite test-single-setup-child-c-1 (test-single-setup-child-c) ()
  (:setup (push :c-1 *test-notepad*))
  (:test (test-1 (ensure t))))

;;; ---------------------------------------------------------------------------

(addtest (test-single-setup)
  test-a-multiple-setup
  (setf *test-notepad* nil)
  (run-test :suite 'test-single-setup-child-a-1 :name 'test-1)
  (run-test :suite 'test-single-setup-child-a-1 :name 'test-2)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-b-single-setup-1
  (setf *test-notepad* nil)
  (run-test :suite 'test-single-setup-child-b-1-ss :name 'test-1)
  (run-test :suite 'test-single-setup-child-b-1-ss :name 'test-2)
  ;; single tests do all the setup so this should be exactly the same
  (ensure-same *test-notepad* '(:b-1 :b :b-1 :b)))

(addtest (test-single-setup)
  test-a-single-setup-2
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-a-1 :do-children? nil
	     :report-pathname nil)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-a-single-setup-3
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-a-1 
	     :run-setup :once-per-suite
	     :do-children? nil
	     :report-pathname nil)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-b-single-setup-2
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-b-1-ss :do-children? nil
	     :report-pathname nil)
  (ensure-same *test-notepad* '(:b-1 :b)))

;;; ---------------------------------------------------------------------------
;;; warning behavior
;;; ---------------------------------------------------------------------------

(deftestsuite test-ignore-warnings (lift-test) ())

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
;;; test-creating-multiple-tests
;;; ---------------------------------------------------------------------------

(deftestsuite test-creating-multiple-tests (lift-test)
  ())

(deftestsuite test-creating-multiple-tests-helper ()
 ()
 (:tests ((ensure-same 1 1)
          (ensure-same 2 2))
         ((ensure-same 3 3))))

(addtest (test-creating-multiple-tests)
  test-1
  (ensure-same (testsuite-test-count 'test-creating-multiple-tests-helper) 2))

;;;;;

(defvar *dynamics-after-setup* :das)

(deftestsuite dynamics-after-setup (lift-test)
  ()
  :setup (setf *test-notepad* nil))

(deftestsuite dynamics-after-setup-helper ()
  ((slot (progn (push :slot *test-notepad*) :slot)))
  :dynamic-variables (*dynamics-after-setup* 
		      (progn (push :dynamics *test-notepad*) :dynamics))
  (:setup (push :setup *test-notepad*) (print (list :tn *test-notepad*))))

(addtest (dynamics-after-setup-helper)
  test-1
  (push :test *test-notepad*)
  (ensure-same *dynamics-after-setup* :dynamics))

(addtest (dynamics-after-setup)
  test-1
  (run-test :suite 'dynamics-after-setup-helper
	    :name 'test-1)
  (ensure-same (reverse *test-notepad*)
	       '(:slot :dynamics :setup :test)))


;;;;;
;;; inherited functions

(deftestsuite test-inherited-functions-helper ()
  ()
  (:function 
   (really? (a b c)
	    (ensure-same (+ a b) c :test '=))))

(deftestsuite test-inherited-functions-pos (test-inherited-functions-helper)
  ()
  (:tests ((really? 1 2 3))
	  ((really? 4 5 9))))
	  
(deftestsuite test-inherited-functions-neg (test-inherited-functions-helper)
  ()
  (:tests ((really? -4 -2 -6))
	  ((really? -1 -1 -2))))

(deftestsuite test-inherited-functions (lift-test)
  ())

(addtest (test-inherited-functions)
  one
  (let ((tr (run-tests :suite 'test-inherited-functions-helper
		       :report-pathname nil)))
    (ensure-same (length (tests-run tr)) 4)
    (ensure-null (failures tr))
    (ensure-null (errors tr))))


;;;;;
;;; slot initialization takes place ONCE

(deftestsuite test-initialize-slots-helper ()
  ((slot (incf *test-notepad*))))

(addtest (test-initialize-slots-helper)
  one
  (ensure t))

(addtest (test-initialize-slots-helper)
  two
  (ensure-null nil))

(deftestsuite test-initialize-slots (lift-test)
  ()
  (:setup (setf *test-notepad* 0)))

(addtest (test-initialize-slots)
  slot-initform-evaluated-every-time
  (let ((tr (run-tests :suite 'test-initialize-slots-helper
		       :report-pathname nil)))
    (ensure-same (length (tests-run tr)) 2)
    (ensure-same *test-notepad* 1 :test '=)))

;;;;;
;;; errors during tests are reported in the test result

(defun cause-an-error ()
  (error "this is an error"))

(deftestsuite test-error-catching (lift-test)
  ())

(deftestsuite test-error-catching-helper-slot-init ()
  ((x (cause-an-error))))

(addtest (test-error-catching-helper-slot-init)
  slot-init
  (ensure t))

(addtest (test-error-catching)
  helper-slot-init
  (let ((result (run-test :suite 'test-error-catching-helper-slot-init
			  :name 'slot-init)))
    ;;?? test not run because error occurred during setup
    (ensure-same 0 (length (lift::suites-run result)) :report "tests run")
    (ensure-same 1 (length (errors result)) :report "errors counted")))

;;;

(deftestsuite test-error-catching-helper-body ()
  ())

(addtest (test-error-catching-helper-body)
  body
  (cause-an-error))

(addtest (test-error-catching)
  helper-body
  (let ((result (run-test :suite 'test-error-catching-helper-body
			  :name 'body)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-setup ()
  ()
  (:setup
   (cause-an-error)))

(addtest (test-error-catching-helper-setup)
  setup
  (ensure t))

(addtest (test-error-catching)
  helper-setup
  (let ((result (run-test :suite 'test-error-catching-helper-setup
			  :name 'setup)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-teardown ()
  ()
  (:teardown
   (cause-an-error)))

(addtest (test-error-catching-helper-teardown)
  teardown
  (ensure t))

(addtest (test-error-catching)
  helper-teardown
  (let ((result (run-test :suite 'test-error-catching-helper-teardown
			  :name 'teardown)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(defvar *test-error-catching-helper*)

(deftestsuite test-error-catching-helper-dynamic-variables ()
  ()
  (:dynamic-variables
   (*test-error-catching-helper* (cause-an-error))))

(addtest (test-error-catching-helper-dynamic-variables)
  dynamic-variables
  (ensure t))

(addtest (test-error-catching)
  helper-dynamic-variables
  (let ((result (run-test :suite 'test-error-catching-helper-dynamic-variables
			  :name 'dynamic-variables)))
    (ensure-same 1 (length (lift::suites-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-equality-test ()
  ()
  (:equality-test
   (cause-an-error)))

(addtest (test-error-catching-helper-equality-test)
  equality-test
  (ensure t))

(addtest (test-error-catching)
  helper-equality-test
  (let ((result (run-test :suite 'test-error-catching-helper-equality-test
			  :name 'equality-test)))
    (ensure-same 0 (length (lift::suites-run result))) ;hmmm
    (ensure-same 1 (length (errors result)))))

;;;;

(deftestsuite test-interaction (lift-test)
  ()
  (:equality-test #'string=))

(addtest (test-interaction)
  run-test-sets-values
  (run-test :suite 'lift-test-ensure-helper :name 'simple-ensure-test-3)
  (ensure-same 
   (symbol-name lift::*last-test-case-name*)
   (symbol-name 'simple-ensure-test-3))
  (ensure-same 
   (symbol-name lift::*last-testsuite-name*)
   (symbol-name 'lift-test-ensure-helper)))

(addtest (test-interaction)
  run-tests-sets-values
  (run-tests :suite 'lift-test-ensure-helper
	     :report-pathname nil)
  (ensure-same 
   (symbol-name lift::*last-testsuite-name*)
   (symbol-name 'lift-test-ensure-helper))
  (ensure-same 
   (symbol-name lift::*last-test-case-name*)
   (symbol-name 'simple-ensure-test-3)))

(addtest (test-interaction)
  run-test-sets-values-nested
  (run-test :suite 'test-interaction :name 'run-tests-sets-values)
  (ensure-same 
   (symbol-name lift::*last-testsuite-name*)
   (symbol-name 'test-interaction))
  (ensure-same 
   (symbol-name lift::*last-test-case-name*)
   (symbol-name 'run-tests-sets-values)))

;;;;

(deftestsuite test-expected-errors (lift-test)
  ()  
  (:dynamic-variables
   (*test-break-on-errors?* nil)
   (*test-break-on-failures?* nil)))

(deftestsuite test-expected-errors-helper ()
  ())

(addtest (test-expected-errors-helper
	  :expected-error t)
  test-1
  (error "this is an error"))

(addtest (test-expected-errors)
  test-passes
  (let ((result (run-tests :suite 'test-expected-errors-helper
			   :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 0 (length (errors result)))
    (ensure-same 1 (length (expected-errors result)))
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *test-expected-errors-helper-2* nil))

(deftestsuite test-expected-errors-helper-2 ()
  ())

(addtest (test-expected-errors-helper-2
	  :expected-error *test-expected-errors-helper-2*)
  test-1
  (error "this is an error"))

(addtest (test-expected-errors)
  test-expected-error-helper-true
  (let* ((*test-expected-errors-helper-2* t)
	 (result (run-tests :suite 'test-expected-errors-helper-2
			   :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 0 (length (errors result)))
    (ensure-same 1 (length (expected-errors result)))
    ))

(addtest (test-expected-errors)
  test-expected-error-helper-false
  (let* ((*test-expected-errors-helper-2* nil)
	 (result (run-tests :suite 'test-expected-errors-helper-2
			   :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))
    (ensure-same 0 (length (expected-errors result)))
    ))

(addtest (test-expected-errors)
  donot-break-on-errors
  ;; this is weird
  ;; I wonder if it's worth trying to abstract "up"
  (let ((*debugger-hook* (lambda (condition hook)
			   (declare (ignore hook))
			   (when (find-restart 'entered-debugger)
			     (invoke-restart 'entered-debugger condition))
			   (invoke-debugger condition))))
    (restart-case
      (let ((result (run-tests :suite 'test-expected-errors-helper 
			       :report-pathname nil
			       :break-on-errors? t)))
	(ensure-same 1 (length (tests-run result)))
	(ensure-same 0 (length (errors result)))
	(ensure-same 1 (length (expected-errors result)))
	)
    (entered-debugger (c)
      (declare (ignore c))
      (ensure-null "We should not be here")))))

;;;;

;;?? these pass but the cliquep test did not seem to be working. Why?
(deftestsuite test-scratchpad-resets (lift-test)
  ())

(deftestsuite test-scratchpad-resets-helper ()
  ()
  (:test
   (test-3 (push :test *test-scratchpad*)))
  (:test
   (test-4 (push :burt *test-scratchpad*))))

(addtest (test-scratchpad-resets)
  run-once-have-one
  (run-test :suite 'test-scratchpad-resets-helper :name 'test-3)
  (ensure-same '(:test) *test-scratchpad*))

(addtest (test-scratchpad-resets)
  run-twice-have-one
  (run-test :suite 'test-scratchpad-resets-helper :name 'test-3)
  (run-test :suite 'test-scratchpad-resets-helper :name 'test-3)
  (ensure-same '(:test) *test-scratchpad*))
 
(addtest (test-scratchpad-resets)
  run-twice-have-one-run-tests
  (run-tests :suite 'test-scratchpad-resets-helper :report-pathname nil)
  (run-tests :suite 'test-scratchpad-resets-helper :report-pathname nil)
  (ensure-same '(:burt) *test-scratchpad*))

;;;;

(deftestsuite test-break-on-failure (lift-test)
  ())

(deftestsuite test-break-on-failure-helper ()
  ()
  ;; :categories (foo bar)
  )

(addtest (test-break-on-failure-helper)
  failing-test
  (ensure-null "this fails"))

(addtest (test-break-on-failure)
  donot-break-on-failures
  (let* ((*test-break-on-failures?* nil)
	 (result (run-tests :suite 'test-break-on-failure-helper
			    :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (failures result)))))

(addtest (test-break-on-failure)
  do-break-on-failures
  (let* ((*test-break-on-failures?* t)
	 (*debugger-hook* (lambda (condition hook)
			    (declare (ignore hook))
			    (when (find-restart 'entered-debugger)
			      (invoke-restart 'entered-debugger condition))
			    (invoke-debugger condition)))
	 (result nil))
    (restart-case
	(setf result (run-tests :suite 'test-break-on-failure-helper
				:report-pathname nil))
      (entered-debugger (c)
	(declare (ignore c))
	(setf *test-scratchpad* t)))
    (ensure-null result)
    (ensure-same *test-scratchpad* t :test 'eq)))

;;;;

(deftestsuite ensure-no-warning (lift-test)
  ())

(deftestsuite ensure-no-warning-helper ()
  ())

(addtest (ensure-no-warning-helper)
  test-1
  (ensure-no-warning (ensure-same (+ 2 2) 4)))

(addtest (ensure-no-warning-helper)
  test-2
  (ensure-no-warning (ensure-same (+ 2 2) 4)
		     (warn "I like math")))

(addtest (ensure-no-warning)
  run-tests
  (let ((result (run-tests :suite 'ensure-no-warning-helper
			   :report-pathname nil)))
    (ensure-same (length (tests-run result)) 2)
    (ensure-same (length (failures result)) 1)
    (ensure-same (length (errors result)) 0)))

;;;;;

(deftestsuite test-suite-with-no-tests-helper ()
  ())

(deftestsuite test-test-suite-with-no-tests (lift-test)
  ()
  (:documentation "Case 168"))

(addtest (test-test-suite-with-no-tests)
  test-1
  (let ((r (run-tests :suite 'test-suite-with-no-tests-helper)))
    (ensure-same (length (tests-run r)) 0)))

;;;;;


(deftestsuite handle-serious-condition (lift-test)
  ()
  (:documentation 
   "LIFT should keep running tests even when a testcase gets a
serious condition. (though maybe there should be an option that
these cancel testing instead.)")
  (:dynamic-variables
   (*test-break-on-errors?* nil)
   (*test-break-on-failures?* nil)))

(deftestsuite handle-serious-condition-helper ()
  ())

(addtest (handle-serious-condition-helper)
  test-1
  (signal 'serious-condition)
  #+(or)
  ;; I expect this to signal an error!
  (make-array (1- most-positive-fixnum)))

(addtest (handle-serious-condition)
  test-1
  (let ((got-condition nil))
    (handler-case 
	(let ((tr (run-test :suite 'handle-serious-condition-helper
			    :name 'test-1)))
	  (ensure-same (length (tests-run tr)) 1)
	  (ensure-null (failures tr))
	  (ensure (errors tr))
	  (ensure-same (test-mode tr) :single))
      (condition (c)
	(setf got-condition c)))
    (ensure-null got-condition)))

;;;;


(deftestsuite test-interrupts (lift-test)
  ())

(deftestsuite test-interrupts-helper ()
  ())

(addtest (test-interrupts-helper)
  test-1
  (push :a *test-notepad*))

#+allegro
(addtest (test-interrupts-helper)
  test-2
  (push :b *test-notepad*)
  (signal 'excl:interrupt-signal))

(addtest (test-interrupts-helper)
  test-3
  (push :c *test-notepad*))

#+allegro
(addtest (test-interrupts)
  test-1
  (let ((*test-notepad* nil))
    (lift:run-tests :suite 'test-interrupts-helper)
    (ensure-same *test-notepad* '(:b :a) :test 'equal)))

#|
;;;;

(deftestsuite test-errors-in-equality-test (lift-test)
  ())

(deftestsuite test-errors-in-equality-test-helper ()
  ())

(addtest (test-errors-in-equality-test-helper
	  :documentation "this is fun")
  test-1
  (ensure-same 1 1 :test (lambda (a b) (/ (- a b)))))

(addtest (test-errors-in-equality-test-helper
	 )
  test-1
  (:documentation "this is fun")
  (ensure-same 1 1 :test (lambda (a b) (/ (- a b)))))


(addtest (test-errors-in-equality-test)
  test-1
  (let ((*test-notepad* nil))
    (lift:run-tests :suite 'test-errors-in-equality-test-helper)
    (ensure-same *test-notepad* '(:b :a) :test 'equal)))


(addtest (report-pathnamexx)
  initial-properties-are-null
  (ensure-null (lift::test-result-properties result))
)

|#

(deftestsuite test-default-initargs-abstract (lift-test)
  ())

(deftestsuite test-default-initargs-parent (test-default-initargs-abstract)
  (a (b 1))
  (:default-initargs
      :a :parent
    :c :inherit))

(addtest (test-default-initargs-parent)
  no-initform
  (ensure-same a :parent))

(addtest (test-default-initargs-parent)
  with-initform
  (ensure-same b 1))

(deftestsuite test-default-initargs-child (test-default-initargs-parent)
  (c)
  (:default-initargs
      :a :child))

(addtest (test-default-initargs-child)
  no-initform-1
  (ensure-same a :child))

(addtest (test-default-initargs-child)
  no-initform-2
  (ensure-same c :inherit))

(defvar *test-default-initargs-helper-var* nil)

(deftestsuite test-default-initargs-helper ()
  (a)
  (:default-initargs
      :a 1))

(addtest (test-default-initargs-helper)
  test-1
  (ensure-same a *test-default-initargs-helper-var*))

(deftestsuite test-default-initargs (test-default-initargs-abstract)
  ())

(addtest (test-default-initargs)
  test-1
  (let* ((*test-default-initargs-helper-var* 1)
	 (r (lift:run-tests :suite 'test-default-initargs-helper
			    :report-pathname nil)))
    (ensure-null (errors r))
    (ensure-null (failures r))))

(addtest (test-default-initargs)
  test-2
  (let* ((*test-default-initargs-helper-var* 2)
	(r (lift:run-tests :suite 'test-default-initargs-helper
			   :report-pathname nil
			   :testsuite-initargs '(:a 2))))
    (ensure-null (errors r))
    (ensure-null (failures r))))

;;;;


(deftestsuite test-dependencies (lift-test)
  ())

(deftestsuite test-dependencies-helper ()
  ()
  )

(addtest (test-dependencies-helper)
  test-a
  (push :a *test-notepad*))

(addtest (test-dependencies-helper)
  test-b
  (push :b *test-notepad*))

(addtest (test-dependencies-helper :depends-on 'test-b)
  test-c
  (push :c *test-notepad*))

(addtest (test-dependencies-helper :depends-on 'test-c)
  test-d
  (push :d *test-notepad*))

(addtest (test-dependencies)
  test-run-tests
  (setf *test-notepad* nil)
  (let ((r (lift:run-tests :suite 'test-dependencies-helper 
			  :report-pathname nil)))
    (ensure (every (lambda (name)
		     (lift::test-case-tested-p 
		      'test-dependencies-helper name :result r))
		   (list 'test-a 'test-b 'test-c 'test-d)))
    (ensure-same (length *test-notepad*) 4)
    (ensure-same *test-notepad* '(:a :b :c :d) :test 'set-equal)))

(addtest (test-dependencies)
  test-run-test-b
  (setf *test-notepad* nil)
  (let ((r (lift:run-test :suite 'test-dependencies-helper 
			  :name 'test-b)))
    (ensure (every (lambda (name)
		     (lift::test-case-tested-p 
		      'test-dependencies-helper name :result r))
		   (list 'test-b)))
    (ensure-same *test-notepad* '(:b) :test 'set-equal)))

(addtest (test-dependencies)
  test-run-test-c
  (setf *test-notepad* nil)
  (let ((r (lift:run-test :suite 'test-dependencies-helper 
			  :name 'test-c)))
    (ensure (every (lambda (name)
		     (lift::test-case-tested-p 
		      'test-dependencies-helper name :result r))
		   (list 'test-b 'test-c)))
    (ensure-same *test-notepad* '(:c :b) :test 'set-equal)))

(addtest (test-dependencies)
  test-run-test-d
  (setf *test-notepad* nil)
  (let ((r (lift:run-test :suite 'test-dependencies-helper 
			  :name 'test-d)))
    (ensure-same (length *test-notepad*) 3)
    (ensure (every (lambda (name)
		     (lift::test-case-tested-p 
		      'test-dependencies-helper name :result r))
		   (list 'test-b 'test-c 'test-d)))
    (ensure-same *test-notepad* '(:c :b :d) :test 'set-equal)))

;;;;

#|

(deftestsuite lift-syntax-checks (lift-test)
  ()
  (:setup
   (eval '(deftestsuite test-xxx ()()))))

(addtest (lift-syntax-checks)
  options-are-plist-1
  (eval '(addtest (test-xxx :a 1 :b 2) test-1 (ensure t)))
  (ensure (find-test-case 'test-xxx 'test-1)))

(addtest (lift-syntax-checks)
  options-not-plist-1
  (eval '(addtest (test-xxx :a 1 :b 2 :c) test-1 (ensure t)))
  (ensure (find-test-case 'test-xxx 'test-1)))

(addtest (lift-syntax-checks)
  options-not-plist-2
  (eval '(addtest (test-xxx :b) test-1 (ensure t)))
  (ensure (find-test-case 'test-xxx 'test-1)))

(addtest (lift-syntax-checks)
  options-not-plist-3
  (eval '(addtest (test-xxx :b 2 :c 3 4 5) test-1 (ensure t)))  
  (ensure (find-test-case 'test-xxx 'test-1)))

|#

;;;;

#|
(deftestsuite this-testsuite-fails (lift-test)
  ())

(addtest (this-testsuite-fails)
  test-1
  (ensure-same (+ 2 2) 3))

(deftestsuite this-testsuite-errors (lift-test)
  ())

(addtest (this-testsuite-errors)
  test-1
  (let ((x 0))
    (ensure (/ (* (+ 2 2) 3) x))))

(deftestsuite this-testsuite-is-generally-bad (lift-test)
  ()
  (:documentation "And that's OK."))

(addtest (this-testsuite-is-generally-bad :documentation "What happens here")
  test-1
;  (:documentation "What happens here")
  (let ((x 0))
    (ensure (/ (* (+ 2 2) 3) x))))

(addtest (this-testsuite-is-generally-bad)
  test-2
  (ensure-same (+ 2 2) 3))

(deftestsuite this-testsuite-cannot-be-made (lift-test)
  ((x (error "dang")))
  (:documentation "And that's the way we like it."))

(addtest (this-testsuite-cannot-be-made :documentation "What happens to this?")
  test-1
  (ensure-same 1 1))

(deftestsuite test-conditions (lift-test)
  ())

(addtest (test-conditions)
  test-1
  (signal 'excl:socket-chunking-end-of-file)
  (ensure-same 1 1))

|#

(defun slurp (pathname)
  (when (probe-file pathname)
    (with-open-file (s pathname :direction :input)
      (loop for line = (read-line s nil :eof) 
	 until (eq line :eof) collect line))))

(deftestsuite test-log-file (lift-test)
  (log-file)
  (:setup
   (setf log-file (format nil "/tmp/~a" (gensym "file-")))
   (when (probe-file log-file)
     (delete-file log-file))))

(deftestsuite test-log-file-helper ()
  (log-file))

(addtest (test-log-file-helper)
  test-1
  (let ((lines (slurp log-file)))
    (ensure (plusp (length lines)))
    (let* ((last-line (first (last lines)))
	   (datum (read-from-string last-line nil nil)))
      (ensure datum)
      (ensure (consp datum))
      (ensure-same (car datum) :start-time-universal))))

(addtest (test-log-file)
  test-1
  (let ((r (lift:run-tests
	    :suite 'test-log-file-helper
	    :report-pathname log-file
	    :testsuite-initargs `(:log-file ,log-file))))
    (ensure-null (errors r))
    (ensure-null (failures r))
    (ensure (plusp (length (tests-run r))))))
