{set-property html yes}
{set-property title LIFT User's Guide}
{set-property style-sheet user-guide}
{set-property docs-package lift}

# LIFT User's Guide

# Table of Contents 

{table-of-contents :start 2 :depth 3}

## Introduction

The LIsp Framework for Testing (LIFT) is a unit and system test tool for LISP. 
Though inspired by [SUnit][] and [JUnit][], it's built with Lisp in mind. 
In LIFT, [testcases][] are organized into hierarchical [testsuites][] each of 
which can have its own [fixture][]. When run, a testcase can succeed, fail, 
or error. LIFT supports randomized testing, benchmarking, profiling, and reporting.

 [testcases]> glossary A testcase is the smallest unit of testing.
 [testsuites]> glossary A testsuite is a group of tests plus their environment plus local variables and configuration settings. 
 [fixture]> glossary The environment in which a testcase runs. This includes code for both setup and teardown.
 
## Overview : our first testsuite

LIFT supports interactive testing so imagine that we type each of the following forms into a file and evaluate them as we go. The comments explain what is going on.

    {recode first-testsuite}
    (in-package #:common-lisp-user)
    (use-package :lift)

    ;;; define an empty testsuite. Deftestsuite is like defclass
    ;; so here we define a testsuite with no super-testsuites and
    ;; no slots
    (deftestsuite lift-examples-1 () ())

    ;;; and add a test to it
    ;; LIFT will add this to the most recently defined testsuite
    ;; we didn't give this test a name so LIFT will name it for us
    (addtest (lift-examples-1)
      (ensure-same (+ 1 1) 2))

    ;;; add another test using ensure-error
    ;; Here we specify the testsuite. 
    (addtest (lift-examples-1)
      (ensure-error (let ((x 0)) (/ x))))

    ;;; add another, slightly more specific test
    (addtest (lift-examples-1)
      (ensure-condition division-by-zero (let ((x 0)) (/ x))))

    ;;; run all the defined tests
    ;; Unless you tell it otherwise, LIFT runs all the testcases
    ;; of the most recently touched testsuite. In this case, thats
    ;; lift-example-1
    (run-tests)

If you don't supply a testcase name, LIFT will give it one. This works for quick interactive testing but makes it hard to find a problem when running regression tests. It's a much better practice to give every testcase a name -- it also makes the testsuite self documenting. 

    {recode first-testsuite extend}
    ;;; here is a testcase with a name
    ;; it fails because floating point math isn't exact.
    (addtest (lift-examples-1)
      floating-point-math
      (ensure-same (+ 1.23 1.456) 2.686))
     
    ;;; We try again using the function almost= for the test of ensure-same
    (addtest (lift-examples-1)
      floating-point-math
      (ensure-same (+ 1.23 1.456) 2.686 :test 'almost=))
    
    ;;; whoopts, we forgot to write almost=! Here's a simple (though not
    ;; very efficient) version
    (defun almost= (a b)
      (< (abs (- a b)) 0.000001))
    
    ;; like run-tests, run-test runs the most recently touched testcase.
    (run-test)

The examples above cover most of LIFT's basics: 

* Use [deftestsuite][] and [addtest][] to define testsuites and testcases.
* In a testcase, use members of the ensure family of macros (like [ensure][], [ensure-same][], and [ensure-condition][]) to specify what is supposed to happen
* Run tests interactively by evaluating them or by calling [run-test][] or [run-tests][]

In what follows, we'll explore LIFT in more depth by looking at test hierarchies and fixtures, randomized testing, and using LIFT for benchmarking and profiling.

## Defining testsuites and adding testcases.

The [deftestsuite] macro defines or redefines a testsuite. Testsuites are CLOS classes and deftestsuite looks a lot like defclass.

    (deftestsuite name (supersuite\*)
        (slotspec\*)
        options\*)

The list of supersuites lets you organize tests into a hierarchy. This can be useful both to share fixtures (i.e., setup and tearcode code) and to organize your testing: different parts of the hierarchy can test different parts of your software. The slotspecs are similar to slotspecs in defclass but with a twist: deftestsuite automatically adds an initarg and accessor for each spec{footnote Though they once did, the slotspecs don't really define slots for the class internally anymore. LIFT keeps track of slot values through a different (slower but more flexible) mechanism.}. You can specify an initial value using a pair rather than needing to specify an initform and these value can use the values of previously defined slots (as if they were being bound in a let* form). Finally, you'll also see below that slot values are immediately available with the body of a test method. These two features make writing tests very simple.

    > (deftestsuite test-slots ()
        ((a 1) (b 2) (c (+ a b)))
        (:test ((ensure-same (+ a b) c))))
    Start: test-slots
    #<Results for test-slots [1 Successful test]>

The example above also shows that you can define tests directly in the deftestsuite form.  This is really handy for unit testing where you don't want the boilerplate to get in the way of the tests! Here is another, more complex example:

    > (deftestsuite test-leap-year-p ()
       ()
       ;; Use :tests to define a list of tests
       (:tests
        ((ensure (leap-year-p 1904)))
        ;; we give this one a name
        (div-by-four (ensure (leap-year-p 2000)))
        ((ensure (leap-year-p 1996))))
       ;; use :test to define one test at a time
       (:test ((ensure-null (leap-year-p 1900))))
       (:test ((ensure-null (leap-year-p 1997)))))

    ;; let's see what we've done
    > (print-tests :start-at 'test-leap-year-p)
    test-leap-year-p (5)
      TEST-1
      div-by-four
      TEST-3
      TEST-4
      TEST-5

So far, our tests have not required any setup or teardown. Let's next look at at a few tests that do. The first example is from the [ADSF-Install][] testsuite. It uses its fixtures setup to make sure that the working directory is empty (so that it is ensured of installing into a clean system).{footnote We'll talk about the :dynamic-variables clause in more detail below}

    (deftestsuite test-asdf-install-basic-installation (test-asdf-install) 
      ()
      (:dynamic-variables 
       (*verify-gpg-signatures* t))
      (:setup 
       (delete-directory-and-files *working-directory* 
           :if-does-not-exist :ignore)))

This next testsuite is from [Log5][]. Though the details aren't important, you can be assured that LIFT will the setup before *every* testcase and the teardown after *every* testcase (even if there is an error).  

    (deftestsuite test-stream-sender-with-stream (test-stream-sender)
     (sender-name
      string-stream
      (sender nil))
     (:setup
      (setf sender-name (gensym)
    	 string-stream (make-string-output-stream)))   
     (:teardown (stop-sender-fn sender-name :warn-if-not-found-p nil))
     :equality-test #'string-equal)

#### Deftestsuite options and arguments

We've already seen two other clauses that deftestsuite supports (:dynamic-variables and :equality-test). Here is the complete list:

* dynamic-variables - specifies how to initialize dynamic-variables within a testsuite
* documentation - used, of all things, for documentation
* equality-test - specifies the default equality-test used by ensure-same and ensure-different. See [*lift-equality-test*][]
* export-p - if true, the testsuite name will be exported
* export-slots - if true, all of the testsuite slots will be exported. It can also be a list of slot names to export
* function - defines a local test function (think of flet or labels)
* random-instance - tells LIFT how to make random examples of things for this suite
* run-setup - tells LIFT when to run setup.
* setup - code for test setup
* teardown - code for testsuite teardown
* test - defines a single test
* tests - defines several tests
* timeout - how long can each test take

Many of these are self-explanatory and we'll look at :random-instance below when we talk about random-testing. This leaves us to discuss :dynamic-variables, :equality-test, :function, :run-setup and :timeout here.

##### Dynamic-variables

It is often the case that you'll want some dynamic variable bound around the body of all of your tests. This is hard to do because LIFT doesn't expose it's mechanism for easy access.{footnote At least, it doesn't expose them yet... One long range plan is to do a better job of building a sort of test metaobject protocol that would make it easier to extend LIFT in new and exciting ways.}. The :dynamic-variables clause lets you specify a list of variables and bindings that LIFT will setup for each testcase. XXX exactly when?

##### Equality-test

To be written.

##### Function

To be written.

##### Run-setup

To be written.

##### Timeout

To be written.

### LIFT and Random testing

To be written.

### Benchmarking with LIFT

To be written.

### Reporting

To be written.

<div class='reference'>
    
## Reference

### Defining Tests

{docs deftestsuite macro}
{docs addtest macro}

### Testing conditions

The following macros can be used outside of LIFT where they will function very much like `assert`. When used in the body of an `addtest` or `deftestsuite` form, however, they will record test failures instead of signaling one themselves.

{docs ensure macro}
{docs ensure-null macro}
{docs ensure-same macro}
{docs ensure-different macro}
{docs ensure-condition macro}
{docs ensure-warning macro}
{docs ensure-error macro}
{docs ensure-cases macro}
{docs ensure-random-cases macro}

### Running tests 

{docs run-test function}
{docs run-tests function}
{docs lift-result}
{docs lift-property}
{docs *test-result* variable}

### Configuring LIFT

{docs *test-ignore-warnings?* variable}
{docs *test-break-on-errors?* variable}
{docs *test-print-length* variable}
{docs *test-print-level* variable}
{docs *test-print-when-defined?* variable}
{docs *test-evaluate-when-defined?* variable}
{docs *test-describe-if-not-successful?* variable}
{docs *test-maximum-time* variable}
{docs *test-print-suite-names* variable}
{docs *test-print-test-case-names* variable}
{docs *lift-equality-test* variable}
{docs *lift-debug-output* variable}

### Introspection

{docs print-tests}
{docs map-testsuites}
{docs testsuites}
{docs testsuite-tests}
{docs find-testsuite}
{docs last-test-status}
{docs suite-tested-p}
{docs testsuite-p}
{docs failures}
{docs errors}

### Random testing

{docs ensure-random-cases}
{docs ensure-random-cases+}
{docs random-instance-for-suite}

{docs ensure-random-cases-failure}

{docs defrandom-instance}
{docs random-element}
{docs random-number}
{docs an-integer}
{docs a-double-float}
{docs a-single-float}
{docs a-symbol}

### Benchmarking and Profiling

{docs measure-time}
{docs measure-space}

### Miscellaneous 

{docs test-mixin}
{docs *current-test*}

</div>

## Indices

### Index of Functions

{docs-index function :sorted t}

### Index of variables

{docs-index variable :sorted t}

### Index of Macros

{docs-index macro}

<hr>

#### Footnotes

{footnotes}


