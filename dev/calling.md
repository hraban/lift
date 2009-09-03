# who calls what / who does what

run-tests
  run-tests-from-file
  run-tests-internal

run-tests-internal
  do-testing-in-environment with testsuite-run

do-testing-in-environment
  setup restart and handler
    testsuite-setup
    do-testing
    testsuite-teardown

do-testing
  (:around setup dynamic variables)
  (:around setup timeout handler)
  call `fn` (either testsuite-run or run-test-internal)

testsuite-run
  for each method 
    run-test-internal
  for each subclass not yet tested, 
    run-tests-internal

run-test-internal
  setup restart and handler
  setup-test
  lift-test -- the actual test code!
  test-case-teardown

run-test
  do-testing-in-environment with run-test-internal

;;;

testsuite-setup never has any methods defined on it

testsuite-teardown and test-case-teardown are the same 
  except for parameter to run-teardown-p




# Stuff

in start-test (result test name)
   (push `(,name ,(current-values test)) (tests-run result))

if fails / errors, will get problem appended 

current-values comes from prototype stuff

use property-list format 
  start-time
  end-time
  time
  space??

:created
:testsuite-setup
:testing



===========


## Older?

run-tests
  run-tests-internal
  or run-tests-from-file

run-test 
  do-testing with run-test-internal

------

run-tests-from-file
  run-tests

run-tests-internal
  make-testsuite
  do-testing with testsuite-run

do-testing (suite)
  testsuite-setup *
  foreach prototype
    initialize-test
    <fn> (= testsuite-run)
  testsuite-teardown *

run-test-internal
  start-test - push, name, value onto test-placeholder *
  setup-test *
    (initialize slots)
  lift-test *
  teardown-test *
  end-test - setf :end-time *
  (add test-data to tests-run of result)

testsuite-run
  foreach method in suite, run-test-internal
  if do-children?, foreach direct-subclass, run-tests-internal
