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
    :initform *profile-style*
    :initarg :profile
    :accessor profile))
  (:documentation "A test suite")
  (:default-initargs
    :run-setup :once-per-test-case))

(defclass process-test-mixin (test-mixin)
  ((maximum-time :initform *test-maximum-time* 
                 :accessor maximum-time
                 :initarg :maximum-time)))

(defclass log-results-mixin (test-mixin)
  ())

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
    :accessor testsuite-initargs)
   (uuid
    :initform nil
    :initarg :uuid
    :accessor result-uuid))
  (:documentation 
"A `test-result` instance contains all of the information collectd by 
LIFT during a test run.")
  (:default-initargs
    :test-interactive? *test-is-being-defined?*
    :real-start-time (get-internal-real-time)
    :real-start-time-universal (get-universal-time)))

(defclass test-problem-mixin ()
  ((test-problem-kind 
    :reader test-problem-kind
    :allocation :class
    :initarg :test-problem-kind)))

(defclass test-error-mixin () 
  ((backtrace :initform nil :initarg :backtrace :reader backtrace)))
  
(defclass test-failure-mixin () 
  ())

(defclass expected-problem-mixin ()
  ((documentation :initform nil 
		  :initarg :documentation
		  :accessor failure-documentation)))

(defmethod print-object ((problem test-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "~a" (test-problem-kind problem))))

(defclass testsuite-problem-mixin (test-problem-mixin)
  ((testsuite :initform nil :initarg :testsuite :reader testsuite)
   (test-method :initform nil :initarg :test-method :reader test-method)
   (test-condition :initform nil
		   :initarg :test-condition 
		   :reader test-condition)
   (test-step :initform nil :initarg :test-step :reader test-step)
   (testsuite-initargs 
    :initform nil :initarg :testsuite-initargs
    :reader testsuite-initargs)))

(defmethod print-object ((problem testsuite-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "TEST-~@:(~A~): ~A in ~A" 
            (test-problem-kind problem) 
            (testsuite problem)
	    (test-method problem))))

(defclass test-expected-failure (expected-problem-mixin 
				 test-failure-mixin testsuite-problem-mixin)
  ())

(defmethod test-problem-kind ((problem test-expected-failure))
  "Expected failure")

(defclass test-failure (test-failure-mixin testsuite-problem-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "failure"))

(defclass test-expected-error (expected-problem-mixin test-error-mixin testsuite-problem-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Expected error"))

(defclass test-error (test-error-mixin testsuite-problem-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Error"))

(defclass test-serious-condition (test-error)
  ()
  (:default-initargs 
   :test-problem-kind "Serious condition"))

(defclass testsuite-error (test-error-mixin testsuite-problem-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite error"))

(defclass testsuite-serious-condition (testsuite-error)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite serious condition"))

(defclass testsuite-failure (test-failure-mixin testsuite-problem-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite failure"))

(defclass testcase-skipped (testsuite-problem-mixin)
  ()
  (:default-initargs
   :test-problem-kind "Test case skipped"))

(defclass testsuite-skipped (testsuite-problem-mixin)
  ()
  (:default-initargs
   :test-problem-kind "Testsuite skipped"))

(defclass test-timeout-failure (test-failure)
  ((test-problem-kind :initform "Timeout" :allocation :class)))

(defclass test-configuration-problem-mixin (test-problem-mixin)
  ((message :initarg :message :reader test-problem-message)))

(defmethod print-object ((problem test-configuration-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "~a: ~a" 
	    (test-problem-kind problem) (test-problem-message problem))))

(defclass test-configuration-failure (test-configuration-problem-mixin test-failure-mixin)
  ())
   
(defclass test-configuration-error (test-configuration-problem-mixin test-error-mixin)
  ())

(defmethod test-problem-kind ((problem test-configuration-problem-mixin))
  "Configuration problem")

(defmethod test-problem-kind ((problem test-configuration-error))
  "Configuration error")

;;;;

(defun testsuite-failures (result)
  (remove-if-not (lambda (p) (typep p 'testsuite-problem-mixin)) (failures result)))

(defun configuration-failures (result)
  (remove-if-not (lambda (p) (typep p 'test-configuration-problem-mixin)) (failures result)))
