#|
(in-package lift-test)

(deftestsuite no-tests ()
  ())

(run-tests :suite 'no-tests)
|#

(in-package #:lift)

(deftestsuite handle-serious-condition (lift-test)
  ())

(deftestsuite handle-serious-condition-helper (lift-test)
  ())

(addtest (handle-serious-condition-helper)
  test-1
  ;; I expect this to fail!
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

(run-tests :suite 'handle-serious-condition)


#|  
       /- ga
   /- c - ga1
p -
   \- c1 - gb
        \- gb1

|#

#+(or)
(print
 (let ((*test-print-test-case-names* t)
      (*test-notepad* nil))
  (run-tests :suite 'child-tests)
  (reverse *test-notepad*))
)

#+(or)
(print
(let ((*test-print-test-case-names* t)
      (*test-notepad* nil))
  (run-tests :suite 'child-tests-once)
  (reverse *test-notepad*))
)
#+(or)
(:parent-setup :child-1-setup :child-1-one :child-1-two :parent-setup
 :child-1-setup :gc-b-1-setup :gc-b-1-one :gc-b-1-two :parent-setup
 :child-1-setup :gc-b-setup :gc-b-one :gc-b-two) 

#|
I think it would make more sense for it to be

(:parent-setup :child-1-setup :child-1-one :child-1-two
 :parent-setup :child-1-setup :gc-b-1-setup :gc-b-1-one :gc-b-1-two
 :parent-setup :child-1-setup :gc-b-setup :gc-b-one :gc-b-setup :gc-b-two) 

i.e., for the gc-b setup for get run before each gc-b test...
|#

(deftestsuite parent-tests ()
  ()
  (:setup (push :parent-setup *test-notepad*)))

(deftestsuite child-tests (parent-tests)
  ()
  (:setup (push :child-setup *test-notepad*)))

(addtest (child-tests)
  one
  (push :child-one *test-notepad*))

(addtest (child-tests)
  two
  (push :child-two *test-notepad*))

(deftestsuite child-tests-once (parent-tests)
  ()
  :run-setup :once-per-suite
  (:setup (push :child-1-setup *test-notepad*)))

(addtest (child-tests-once)
  one
  (push :child-1-one *test-notepad*))

(addtest (child-tests-once)
  two
  (push :child-1-two *test-notepad*))

;;;

(deftestsuite grandchild-a-tests (child-tests)
  ()
  (:setup (push :gc-a-setup *test-notepad*)))

(addtest (grandchild-a-tests)
  one
  (push :gc-a-one *test-notepad*))

(addtest (grandchild-a-tests)
  two
  (push :gc-a-two *test-notepad*))

(deftestsuite grandchild-a-tests-once (child-tests)
  ()
  :run-setup :once-per-suite
  (:setup (push :gc-a-1-setup *test-notepad*)))

(addtest (grandchild-a-tests-once)
  one
  (push :gc-a-1-one *test-notepad*))

(addtest (grandchild-a-tests-once)
  two
  (push :gc-a-1-two *test-notepad*))

;;;

(deftestsuite grandchild-b-tests (child-tests-once)
  ()
  (:setup (push :gc-b-setup *test-notepad*)))

(addtest (grandchild-b-tests)
  one
  (push :gc-b-one *test-notepad*))

(addtest (grandchild-b-tests)
  two
  (push :gc-b-two *test-notepad*))

(deftestsuite grandchild-b-tests-once (child-tests-once)
  ()
  :run-setup :once-per-suite
  (:setup (push :gc-b-1-setup *test-notepad*)))

(addtest (grandchild-b-tests-once)
  one
  (push :gc-b-1-one *test-notepad*))

(addtest (grandchild-b-tests-once)
  two
  (push :gc-b-1-two *test-notepad*))
