(in-package #:lift-test)

;; test-freetext-indexing-abstract
(deftestsuite parent-1 ()
  ()
  (:run-setup :once-per-suite)
)

;; test-federation-basic
(deftestsuite parent-2 ()
  ()
  (:run-setup :once-per-suite)
  (:setup
   (push :p2 *test-notepad*)))

;; test-fed-freetext
(deftestsuite child-1 (parent-1 parent-2)
  ()
  (:run-setup :once-per-suite)
  (:setup
   (push :c1 *test-notepad*)))

(addtest (child-1)
  test-1
  (push :c1a *test-notepad*))

#+(or)
(deftestsuite child-3 (parent-2)
  ()
  (:run-setup :once-per-suite)
  (:setup
   (push :c3 *test-notepad*)))

(deftestsuite child-2 (child-1)
  ()
  (:run-setup :once-per-suite)
  (:setup
   (push :c2 *test-notepad*)))

(addtest (child-2)
  test-1
  (push :c2a *test-notepad*))

#|
(setf *test-notepad* nil)

(run-tests :suite 'child-2)

(run-tests :suite 'parent-2)

|#
#|

;;;;;;;;;;;;;;;;;;

(deftestsuite setup-and-slots-hierarchy-parent ()
  ((slot-parent (progn (push :slot-parent *test-scratchpad*) :a)))
  :setup (push :setup-parent *test-scratchpad*)
  :teardown (push :teardown-parent *test-scratchpad*))

(deftestsuite setups-and-slots-hierarchy-child
    (setup-and-slots-hierarchy-parent)
  ((slot-child (progn (push :slot-child *test-scratchpad*) :a)))
  :setup (push :setup-child *test-scratchpad*)
  :teardown (push :teardown-child *test-scratchpad*))



;;;;;;;;;;;;;;;;;;;;;

(run-test :break-on-errors? t)

(deftestsuite warnings-and-errors ()
  ())

(defun warnings-and-errors-function (mode)
  (ecase mode
    (:warn (warn "this is a warning all by itself"))
    (:error (error "this is an error all by itself"))
    (:warn-error (warn "first we warn") (error "then we error"))
    (:error-warn (error "first we error") (warn "then we warn"))))

(addtest (warnings-and-errors)
  warning-does-not-hide-error-1
  (ensure-error (warnings-and-errors-function :warn-error)))

(addtest (warnings-and-errors)
  warning-does-not-hide-error-2
  (ensure-warning (warnings-and-errors-function :warn-error)))

(addtest (warnings-and-errors)
  just-error
  ;; the error is eaten even when you run the test with break-on-errors? t
  (ensure-error (warnings-and-errors-function :error)))

(addtest (warnings-and-errors)
  just-error
  ;; the error is eaten even when you run the test with break-on-errors? t
  (warnings-and-errors-function :error))

;;;;;;


(deftestsuite tests-20081021b ()
  ((foo "123")
   (bar "456"))
  (:setup
   (print (list :te-a lift::*test-environment*))
   (print (list foo bar))))

(addtest (tests-20081021b)
  test-1
  (print foo))

;;;;

|#

(deftestsuite test-boe? (lift-test)
  ())

(addtest (test-boe?)
  test-1
  (print "start")
  (error "error"))


#|
we do setup with progn method combination. When we do the setup for a 
child, we also do setup for the parent. 

but not the teardown?

|#

;; helpers
(deftestsuite test-ss&t-helper ()
  ()
  (:setup    (push :s *test-notepad*))
  (:teardown (push :t *test-notepad*))
  (:run-setup :once-per-suite))

(deftestsuite test-ss&t-child-a (test-ss&t-helper) () 
  (:setup    (push :a-s *test-notepad*))
  (:teardown (push :a-t *test-notepad*))
  (:test (test-1 (push :a-1 *test-notepad*) (ensure t)))
  (:test (test-2 (push :a-2 *test-notepad*) (ensure t))))

(deftestsuite test-ss&t-child-b (test-ss&t-helper) () 
  (:setup    (push :b-s *test-notepad*))
  (:teardown (push :b-t *test-notepad*))
  (:test (test-1 (push :b-1 *test-notepad*) (ensure t)))
  (:test (test-2 (push :b-2 *test-notepad*) (ensure t))))

(addtest (test-single-setup)
  ss&t-1
  (setf *test-notepad* nil)
  (lift:run-tests  :suite 'test-ss&t-helper 
		   :report-pathname nil
		   :result (make-test-result 'test-singe-setup :multiple)
		   )
  (ensure-same '(:s :b-s :b-1 :b-2 :b-t :a-s :a-1 :a-2 :a-t :t)
	       (reverse *test-notepad*)))