;;;-*- Mode: Lisp; Package: lift -*-

(in-package #:lift)

;;; ---------------------------------------------------------------------------
;;; classes
;;; ---------------------------------------------------------------------------

(defclass test-mixin ()
  ((name :initform nil :initarg :name :accessor name :reader testsuite-name)
   (run-setup :reader run-setup :initarg :run-setup)
   (done-setup? :initform nil :reader done-setup?)
   (done-dynamics? :initform nil :reader done-dynamics?)
   (current-method :initform nil :accessor current-method)
   (log-file :initform nil :initarg :log-file :reader log-file)
   (test-data :initform nil :accessor test-data)
   (profile 
    :initform nil
    :initarg :profile
    :accessor profile))
  (:documentation "A test suite")
  (:default-initargs
    :run-setup :once-per-test-case))

(defclass test-result ()
  ((results-for :initform nil 
		:initarg :results-for 
		:accessor results-for)
   (tests-run :initform nil :accessor tests-run)
   (suites-run :initform nil :accessor suites-run)
   (failures :initform nil :accessor failures)
   (expected-failures :initform nil :accessor expected-failures)
   (errors :initform nil :accessor errors)
   (expected-errors :initform nil :accessor expected-errors)
   (skipped-test-cases :initform nil :accessor skipped-test-cases)
   (skipped-testsuites :initform nil :accessor skipped-testsuites)
   (test-mode :initform :single :initarg :test-mode :accessor test-mode)
   (test-interactive? :initform nil 
                      :initarg :test-interactive? :accessor test-interactive?)
   (real-start-time :initarg :real-start-time :reader real-start-time)
   (start-time :accessor start-time :initform nil)
   (end-time :accessor end-time)
   (real-end-time :accessor real-end-time)
   (real-start-time-universal
    :initarg :real-start-time-universal :reader real-start-time-universal)
   (start-time-universal :accessor start-time-universal :initform nil)
   (end-time-universal :accessor end-time-universal)
   (real-end-time-universal :accessor real-end-time-universal)
   (properties :initform nil :accessor test-result-properties)
   (current-step :initform :created :accessor current-step)
   (testsuite-initargs 
    :initform nil
    :initarg :testsuite-initargs
    :accessor testsuite-initargs))
  (:documentation 
"A `test-result` instance contains all of the information collectd by 
LIFT during a test run.")
  (:default-initargs
    :test-interactive? *test-is-being-defined?*
    :real-start-time (get-internal-real-time)
    :real-start-time-universal (get-universal-time)))

(defclass test-problem-mixin ()
  ((testsuite :initform nil :initarg :testsuite :reader testsuite)
   (test-method :initform nil :initarg :test-method :reader test-method)
   (test-condition :initform nil
		   :initarg :test-condition 
		   :reader test-condition)
   (test-problem-kind :reader test-problem-kind :allocation :class)
   (test-step :initform nil :initarg :test-step :reader test-step)))

(defmethod print-object ((problem test-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "TEST-~@:(~A~): ~A in ~A" 
            (test-problem-kind problem) 
            (name (testsuite problem))
	    (test-method problem))))

(defclass generic-problem (test-problem-mixin)
  ((test-problem-kind :initarg :test-problem-kind
		      :allocation :class)))

(defclass expected-problem-mixin ()
  ((documentation :initform nil 
		  :initarg :documentation
		  :accessor failure-documentation)))

(defclass test-expected-failure (expected-problem-mixin generic-problem)
  ())

(defmethod test-problem-kind ((problem test-expected-failure))
  "Expected failure")

(defclass test-failure (generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "failure"))

(defclass test-error-mixin (generic-problem) 
  ((backtrace :initform nil :initarg :backtrace :reader backtrace)))
  
(defclass test-expected-error (expected-problem-mixin test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Expected error"))

(defclass test-error (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Error"))

(defclass test-serious-condition (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Serious condition"))

(defclass testsuite-error (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite error"))

(defclass testsuite-serious-condition (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite serious condition"))

(defclass testsuite-failure (generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite failure"))

(defclass testcase-skipped (generic-problem)
  ()
  (:default-initargs
   :test-problem-kind "Test case skipped"))

(defclass testsuite-skipped (generic-problem)
  ()
  (:default-initargs
   :test-problem-kind "Testsuite skipped"))

(defclass process-test-mixin (test-mixin)
  ((maximum-time :initform *test-maximum-time* 
                 :accessor maximum-time
                 :initarg :maximum-time)))

(defclass test-timeout-failure (test-failure)
  ((test-problem-kind :initform "Timeout" :allocation :class)))

