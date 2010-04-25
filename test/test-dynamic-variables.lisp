(in-package #:lift-test)

(defvar *a* 1)

(deftestsuite test-dynamic-variables (lift-test)
  ())

(deftestsuite test-dynamic-variables-1 (test-dynamic-variables)
  ()
  (:dynamic-variables (*a* 2))
  (:test (test-1 (ensure-same *a* 2))))

(deftestsuite test-dynamic-variables-2 (test-dynamic-variables)
  ()
  (:test (test-1 (ensure-same *a* 1)))
  (:test (test-2 (ensure *a*))))

;;;; syntax

(deftestsuite test-dynamic-variables-syntax-1 (test-dynamic-variables)
  ()
  (:dynamic-variables (*a* 0) (*b* 1))
  (:test (test-1 (ensure-same (list :dv *a* *b*) '(:dv 0 1))))
  )

(deftestsuite test-dynamic-variables-syntax-2 (test-dynamic-variables)
  ()
  (:dynamic-variables *a* *b*)
  (:test (test-1 (ensure-same (list :dv *a* *b*) '(:dv nil nil))))
  )

;;;;

(deftestsuite dynamic-variables-helper-1 ()
  ()
  (:documentation "It's important that the dynamic variable is not
marked as special in the global environment.")
  (:dynamic-variables *unknown-dynamic-variable*))

#+(or)
(deftestsuite dynamic-variables-helper-2 (dynamic-variables-helper-1)
  ((my-slot *unknown-dynamic-variable*)))

(deftestsuite dynamic-variables-helper-2 (dynamic-variables-helper-1)
  (my-slot)
  (:setup
   (setf my-slot *unknown-dynamic-variable*)))

(addtest (dynamic-variables-helper-2)
  try-it
  (ensure (+ 1 2)))

(deftestsuite test-non-special-dynamic-variables (test-dynamic-variables)
  ())

(addtest (test-non-special-dynamic-variables)
  try-it
  (let ((r (run-tests :suite 'dynamic-variables-helper-2)))
    (ensure-same (length (tests-run r)) 1)
    (ensure-null (lift::errors r))
    (ensure-null (lift::failures r))))

;;;;

(deftestsuite test-dynamic-variables-helper-parent ()
  ()
  (:dynamic-variables
   (*tdvhp* 1)))

(addtest (test-dynamic-variables-helper-parent)
  test-1
  (ensure-same *tdvhp* 1))

(deftestsuite test-dynamic-variables-helper-other-parent ()
  ()
  (:dynamic-variables
   (*tdvhp* 3)))

(deftestsuite test-dynamic-variables-helper-child (test-dynamic-variables-helper-parent)
  ()
  (:dynamic-variables
   (*tdvhp* 2)))

(addtest (test-dynamic-variables-helper-child)
  test-1
  (ensure-same *tdvhp* 2))

(deftestsuite test-dynamic-variables-helper-two-parents-a
    (test-dynamic-variables-helper-parent test-dynamic-variables-helper-other-parent)
  ())

(addtest (test-dynamic-variables-helper-two-parents-a)
  test-1
  (ensure-same *tdvhp* 1))

(deftestsuite test-dynamic-variables-helper-two-parents-b
    (test-dynamic-variables-helper-other-parent test-dynamic-variables-helper-parent)
  ())

(addtest (test-dynamic-variables-helper-two-parents-b)
  test-1
  (ensure-same *tdvhp* 3))

;;;

(deftestsuite test-dynamic-variables-inheritance (test-dynamic-variables)
  ())

(addtest (test-dynamic-variables-inheritance)
  test-1
  (let ((r (run-tests :suite 'test-dynamic-variables-helper-child)))
    (ensure-same (length (tests-run r)) 1)
    (ensure-null (lift::errors r))
    (ensure-null (lift::failures r))))

(addtest (test-dynamic-variables-inheritance)
  test-two-parents-1
  (let ((r (run-tests :suite 'test-dynamic-variables-helper-two-parents-a)))
    (ensure-same (length (tests-run r)) 1)
    (ensure-null (lift::errors r))
    (ensure-null (lift::failures r))))

(addtest (test-dynamic-variables-inheritance)
  test-two-parents-2
  (let ((r (run-tests :suite 'test-dynamic-variables-helper-two-parents-b)))
    (ensure-same (length (tests-run r)) 1)
    (ensure-null (lift::errors r))
    (ensure-null (lift::failures r))))
