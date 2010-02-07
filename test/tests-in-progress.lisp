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


;;;



#|

* make sure dependencies exist (test-dependencies-helper :depends-on 'test-2)

|#


*test-notepad*
