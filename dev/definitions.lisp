;;;-*- Mode: Lisp; Package: lift -*-

(in-package #:lift)

;;; ---------------------------------------------------------------------------
;;; global environment thingies
;;; ---------------------------------------------------------------------------

(defvar *hostname* nil)

(defvar *current-user* 
  (first (last (pathname-directory (user-homedir-pathname)))))

(defparameter *log-tag* nil
  "If set, this is used to tag generated log information.")

(defvar *current-testsuite-name* nil)
(defvar *current-test-case-name* nil)

(defvar *last-testsuite-name* nil)
(defvar *last-test-case-name* nil)

(defvar *test-is-being-defined?* nil)
(defvar *test-is-being-compiled?* nil)
(defvar *test-is-being-loaded?* nil)
(defvar *test-is-being-executed?* nil)

(defvar *test-maximum-error-count* nil
  "The maximum numbers of errors to allow during a [run-tests][].

If `*test-maximum-error-count*` is nil, then a call to run-tests 
will continue regardless of the number of errors. If it a positive
integer, then run-tests will stop as soon as the number of test-errors
if greater than or equal to its value. Setting this to some small 
value can help prevent running lengthly test-suites when there are many
errors. See also [\\*test-maximum-failure-count\\*][].")

(defvar *test-maximum-failure-count* nil
  "The maximum numbers of failures to allow during a [run-tests][].

If `*test-maximum-failure-count*` is nil, then a call to run-tests 
will continue regardless of the number of failures. If it a positive
integer, then run-tests will stop as soon as the number of test-failures
if greater than or equal to its value. Setting this to some small 
value can help prevent running lengthly test-suites when there are many
failures. See also [\\*test-maximum-error-count\\*][].")

(defvar *test-maximum-time* 2
  "Maximum number of seconds a process test is allowed to run before we give up.")

(defvar *test-break-on-errors?* nil)
(defvar *test-break-on-failures?* nil)
(defvar *test-run-subsuites?* t)

(defparameter *test-ignore-warnings?* nil
  "If true, LIFT will not cause a test to fail if a warning occurs while
the test is running. Note that this may interact oddly with ensure-warning.")
(defparameter *test-print-when-defined?* nil)
(defparameter *test-evaluate-when-defined?* t)
(defparameter *test-scratchpad* nil
  "A place to put things. This is set to nil before every test.")
(defparameter *test-notepad* nil
  "Another place to put things \(see {ref *test-scratchpad*}\).")

(defparameter *lift-equality-test* 'equal
  "The function used in ensure-same to test if two things are equal. If metatilities is loaded, then you might want to use samep.")

(defvar *test-describe-if-not-successful?* nil
  ;; Was t, but this behavior was extremely annoying since each
  ;; time a test-restul appears in a stack backtrace it is printed
  ;; over many unstructured lines.
  "If true, then a complete test description is printed when there are any test warnings or failures. Otherwise, one would need to explicity call describe.")

(defvar *test-print-length* :follow-print
  "The print-length in effect when LIFT prints test results. It works exactly like `*print-length*` except that it can also take on the value :follow-print. In this case, it will be set to the value of  `*print-length*`.")
(defvar *test-print-level* :follow-print
  "The print-level in effect when LIFT prints test results. It works exactly like `*print-level*` except that it can also take on the value :follow-print. In this case, it will be set to whatever `*print-level*` is.")

(defparameter *skip-tests* nil
  "A lift of test-suites and (testsuite test-case) pairs that LIFT will ignore
during calls to run-tests.")

(defvar *test-result* nil
  "Set to the most recent test result by calls to run-test or run-tests.")

(defvar *test-metadata* (list)
  "A place for LIFT to put stuff.")

(defvar *current-test* nil
  "The current testsuite.")

(defvar *testsuite-test-count* nil
  "Temporary variable used to 'communicate' between deftestsuite and addtest.")

(defvar *lift-dribble-pathname* nil
  "If bound, then test output from run-tests will be sent to this file in  
in addition to *lift-standard-output*. It can be set to nil or to a pathname.")

(defvar *lift-report-pathname* nil
  "If bound to a pathname or stream, then a summary of test information will
be written to it for later processing. It can be set to:

* `nil` - generate no output
* pathname designator - send output to this pathname
* `t` - send output to a pathname constructed from the name of the system 
being tested (this only works if ASDF is being used to test the system).

As an example of the last case, if LIFT is testing a system named ...
")

(defvar *lift-standard-output* *standard-output*
  "Output from tests will be sent to this stream. If can set to nil or 
to an output stream. It defaults to *standard-output*.")

(defvar *lift-if-dribble-exists* :append
  "Specifies what to do to any existing file at *lift-dribble-pathname*. It 
can be :supersede, :append, or :error.")

(defvar *test-show-expected-p* t)

(defvar *test-show-details-p* t)

(defvar *test-show-code-p* t)
 
(defvar *current-definition* nil
  "An associative-container which saves interesting information about
the thing being defined.")

(defvar *code-blocks* nil)

(defvar *deftest-clauses*
  '(:setup :teardown :test :documentation :tests :export-p :export-slots
    :run-setup :dynamic-variables :equality-test :categories :function))

;;; ---------------------------------------------------------------------------
;;; Error messages and warnings
;;; ---------------------------------------------------------------------------

(defparameter +lift-test-name-not-supplied-with-test-class+
  "if you specify a test-class, you must also specify a test-name.")

(defparameter +lift-test-class-not-found+
  "test class '~S' not found.")

(defparameter +lift-confused-about-arguments+
  "I'm confused about what you said?!")

(defparameter +lift-no-current-test-class+
  "There is no current-test-class to use as a default.")

(defparameter +lift-could-not-find-test+
  "Could not find test: ~S.~S")

(defparameter +run-tests-null-test-case+
  "There is no current testsuite (possibly because 
   none have been defined yet?). You can specify the 
   testsuite to test by evaluating (run-tests :suite <suitename>).")

(defparameter +lift-unable-to-parse-test-name-and-class+ 
  "")


(defvar *measures* nil
  "A list of defineded measures")

(defparameter *log-path*
  (asdf:system-relative-pathname 'lift "benchmark-data/benchmarks.log"))

(defvar *count-calls-p* nil)

(defvar *profile-style* nil
  "Sets the default profiling style to :time, :space, or nil (for no profiling).")

(defvar *functions-to-profile* nil)

(defvar *profiling-threshold* nil)

