(in-package lift)

(list lift::*test-is-being-compiled?*
      lift::*test-is-being-defined?*
      lift::*test-is-being-executed?*
      lift::*test-is-being-loaded?*)

(setf lift::*test-is-being-compiled?* nil
      lift::*test-is-being-defined?* nil
      lift::*test-is-being-executed?* nil
      lift::*test-is-being-loaded?* nil)

Simple MCL GUI

may need to change teardownn-test to standard method combination and use :after. See
(defmethod teardown-test :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'teardown-test))

randomly change order of tests

sort results so that report is always in same order




(deftestsuite test-addition ()
  (a b)
  (:cases (:map (a '(1 2 3 4 5))
                (b '(9 8 7 6 5))))
  (:test ((ensure-same (+ a b) 10 :test '=))))

(deftestsuite test-addition ()
  (a b)
  (:cases (a '(1 2 3 4 5))
          (b '(9 8 7 6 5)))
  (:test ((ensure-same (+ a b) (+ b a) :test '=))))

ok 1. Handle simple cases clause -> initialize-prototypes
ok 2. Handle run-time evaluation / creation of prototypes
  fool with containers
ok 3. Look at quickcheck and think a bit
4. Determine what a paper on this might look like.

Can't use 'NAME SINGLE-SETUP? DONE-SETUP? PROTOTYPES' as test slots
  should be able to now

make-test-result seems a bit silly too

;;?? Clearly a WIP
(defun testing-interactively-p ()
  (values nil)
  #+Ignore
  (and *test-is-being-defined?*
       (not (or *test-is-being-compiled?*
                *test-is-being-loaded?*))))

test-suite-p, etc could be defined more cleanly if the class one was 'definitive'

Instead of class properties, use the MOP

tinaa lift

new manual

If you define a test suite and a test and then redefine the test-suite, 
  and then try to (run-test), you'll get an error. Should give a nice message

~ need set of lift meta tests and set of lift "UI" tests

save running times??
pending and run-pending method

more introspection
  finding suites and tests

sharing setup / teardown
  accumulating tests

~ - non-deterministic
  e.g., checking for a socket, expect failure occasionally

load testing

testing the test
  donÕt continue when failure option

cache previous results and only rerun failed tests

stochastic tests

non-determininistic tests

[doc] ensure's should have a documentation clause

simplify test-suite-p

need a better error message for when a superclass that DNE or is not a test-class
  is used.

?should use same tricks as define-debugging-class to mangle the name

shouldn't teardown-test be an :after

when using break-on-errors? you should have restarts to allow to cancel out
  completely, or keep going with the next test or whatever

when defining a test with addtest, if no current test case, or if named
  case doesnt't exist, offer to create it (use a restart or whatever like in debug).

remove-test should print a nicer message if the test you're trying to undef DNE

remove-test needs to clean up data tables too

with-test should take optional test-class

clean up all the tables and stuff

add indentation to list-tests (optional) to indicate class inclusion

with-test needs error handling

add documentation (at the least to the public interface)

could format list-tests more nicely

Capture errors and have various fun restarts

should allow error to specify which error it should get

error check addtest if class dne or not test-mixin

Improve error messages

mark tests as known-failures?
  persistence...


(deftestsuite test-a ()
  (a)
  (:cases (a '(1 2))))

(defmethod initialize-prototypes :after ((test test-plus))
  (with-test-slots (a b)
    (setf (prototypes test)
          (list
           (list (let* ((a 1))
                   (cons 'a a))
                 (let* ((a 2))
                   (cons 'a a)))))))

(defmethod initialize-prototypes :after ((test test-plus))
  (with-test-slots (a b)
    (setf (prototypes test)
          (list
           (list (let* ((a 1))
                   (cons 'a a))
                 (let* ((a 2))
                   (cons 'a a)))))))

(defmethod initialize-prototypes :after ((test test-plus))
  (with-test-slots (a b)
    (setf (prototypes test)
          (let* ((a 0) (b 0))
            (list
             (list (cons 'a a) (cons 'b b)))))))

(deftestsuite test-a ()
  ((a '(1 2))))

(deftestsuite test-a ()
  (a)
  (:cases (a '((1 2)))))

(addtest (test-a)
  (format t "~%~A" a))
  
(deftestsuite test-b (test-a)
  (b)
  (:cases (b '(4 5 6))))

(addtest (test-b)
  (format t "~%  ~A x ~A" a b))


(deftestsuite test-e ()
  (a))

(defmethod initialize-prototypes :after ((test test-e))
  (setf (prototypes test)
        (list
         (list (cons 'a 1))
         (list (cons 'a 2)))))

(addtest (test-e)
  (format t "~%~A" a))
  
(deftestsuite test-f (test-e)
  (b))

(defmethod initialize-prototypes :after ((test test-f))
  (setf (prototypes test)
        (list
         (list (cons 'b 4))
         (list (cons 'b 5))
         (list (cons 'b 6)))))

(addtest (test-f)
  (format t "~%  ~A x ~A" a b))

1
  1 4
  1 5
  1 6
2 
  2 4
  2 5
  2 6


(defgeneric setup (suite)
  (:documentation "Setup at the testsuite-level")
  (:method ((suite test-mixin))
           (values)))

(defgeneric testsuite-teardown (suite)
  (:documentation "Cleanup at the testsuite level.")
  (:method ((suite test-mixin))
           (values)))

(defgeneric testsuite-run (suite result)
  (:documentation "Run the cases in this suite and it's children."))

(defgeneric setup-test (test-case)
  (:documentation "Setup for a test-case. By default it does nothing."))

(defgeneric teardown-test (test-case)
  (:documentation "Tear-down a test-case. By default it does nothing.")
  (:method-combination progn :most-specific-first))

(defgeneric testsuite-methods (test-case)
  (:documentation "Returns a list of the test methods defined for test. I.e.,
the methods that should be run to do the tests for this test."))


run-test-internal
  start-test
  setup-test
    funcall test-method
    teardown-test
    end-test


;; put loop here?
run-test-internal
  start-test
--> insert loop
  setup-test
    funcall test-method
    teardown-test
<-- insert loop
    end-test


;; here is better but will need to save off more information for results
;; but this fails for child tests
run-test-internal
--> insert loop
  start-test
  setup-test
    funcall test-method
    teardown-test
    end-test
<-- insert loop


;; so it needs to be here
testsuite-run
--> insert loop
  run each method of this suite using run-test-internal
  run child suites
<-- insert loop



start-test
  push method name on tests-run of the result

setup-test
  initialize-test
  plus after methods

initialize-test
  initializes slots

funcall test-method
  runs the test

teardown-test
  test teardown plus remove variables

end-test
  nada

;;; ---------------------------------------------------------------------------

Simplest case may be to give test-mixin a slot to hold initial values.
For 'regular' tests, this is what it is. For more complex tests, the values
are lists (need to differentiate between lists and multiple prototypes).

Then all tests work the same way. 

The questions for the protocol are standard iteration quesionts: what is
the next value? and are we done yet?



ok - Probably don't need to define accessors or initargs since we're using test-environment
ok - I don't think we need *test-result* -- it's not used
  I think it's a holdover from JUnit
ok - use test-code table to prevent some duplication (or require names)
ok - when name supplied, don't add "test-" to it
ok - get rid of print-test-result {s} and make better use of print-object and describe-object
ok - When adding multiple tests, need to store their names immediately (i.e.,
  before going on to parse the next test).
obsolete - Improve error/warning for duplicate code/names
ok - run-tests works OK; run-test semi; evaluating an addtest does not
ok - need to store more in the test results to see what values were used, etc.
ok - Screw run-test being a macro
ok - Map case keywords to functions (think of tapir). Each of :map, :cross, etc. 
  is handled separately. This will make it easy to extend and build big monsters
  Only "hard" part is correctly combining the conses.
ok - This prototype stuff at compile time is hard... maybe it would be easier to do it
  at run time (using stack / CPL to help...)
ok - The problem with not using slots of the test class is that I need to duplicate
  a bunch of functionality like inherited slots to make with-test-slots work
ok - I don't think I'm using *test-do-children?*, etc. correctly either
ok - Note: LIFT does not use the slots of the instance of the test-class; all looks 
  are via test-environment-value
  (I no longer remember why this was a good idea)
  (maybe don't even need the slots... just the defs)
ok - Get (deftestsuite test-addition ()
      (a b)
      (:cases (:map (a '(1 2 3 4 5))
                    (b '(9 8 7 6 5))))
      (:test ((ensure-same (+ a b) 10 :test '=))))
To work... Standardize-Cases-Form is broken
ok - parse-test-name-and-class is probably wacked b/c I changed to *current-suite-class-name*
  removed
ok - Use multiple processes to handle run away tests
  get reporting to work correctly
ok - the whole number of tests thing is stupid and wacky
something wrong with loading or compiling addtest, the numbers don't get
  incremented properly.
  testsuite-test-count, number-of-tests
  change to getting count at start of deftestsuite and then add 1 to a special 
  for each
no - use dynamic scope for *test-is-being-compiled?* rather than unwind-protect
