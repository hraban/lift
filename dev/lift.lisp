;;;-*- Mode: Lisp; Package: LIFT -*-

#| simple-header

Copyright (c) 2001-2003 Gary Warren King (gwking@cs.umass.edu) 

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions: 

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE. 

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :lift)
    (defpackage lift
      (:use common-lisp mopu))))

(in-package lift)

(export '(test-mixin
          test-suite-p
          
          deftest
          deftestsuite
          addtest
          remove-test
          run-test
          run-tests
          
          ;; Variables
          *test-ignore-warnings?*
          *test-break-on-errors?*
          *test-print-length*
          *test-print-level*
          *test-print-when-defined?*
          *test-evaluate-when-defined?*
          *test-describe-if-not-successful?*
          
          ;;?? not implemented
          ;; *test-select-listener-on-problem?*    
          
          *test-scratchpad*
          
          *lift-equality-test*
          *test-print-length*
          *test-print-level*
          *test-output*
          
          ;; Other
          ensure
          ensure-same
          ensure-condition
          ensure-warning
          ensure-error
          
          ;;?? Not yet
          ;; with-test
          
          list-tests
          
          testsuites
          testsuite-tests
          ))

;;; ---------------------------------------------------------------------------
;;; shared stuff
;;; ---------------------------------------------------------------------------

(declaim (inline length-1-list-p)) 
(defun length-1-list-p (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;;; ---------------------------------------------------------------------------

(defmacro defclass-property (property &optional (default nil default-supplied?))
  "Create getter and setter methods for 'property' on symbol's property lists." 
  (let ((real-name (intern (format nil "~:@(~A~)" property) :keyword)))
    `(progn
       (defmethod ,property ((class-name symbol))
          (get class-name ,real-name ,@(when default-supplied? (list default))))
       (defmethod (setf ,property) (value (class-name symbol))
         (setf (get class-name ,real-name) value)))))

(defvar *automatic-slot-accessors?* nil)
(defvar *automatic-slot-initargs?* nil)
(defvar *clos-slot-options* 
  '(:initform :initarg :reader :writer 
    :accessor :documentation :type
    :allocation))

;;; ---------------------------------------------------------------------------

(defun parse-brief-slot
       (slot &optional
	     (automatic-accessors? *automatic-slot-accessors?*)
	     (automatic-initargs? *automatic-slot-initargs?*)
	     conc-name
             (conc-separator "-"))
  "Returns a verbose-style slot specification given a brief style, consisting of
a single symbol, the name of the slot, or a list of the slot name, optional
initform, optional symbol specifying whether there is an initarg, reader, or
accessor, and optional documentation string.  The specification of initarg,
reader and accessor is done by the letters I, R and A, respectively; to specify
none of those, give a symbol containing none of those letters, such as the
symbol *.  This function is used in the macro `defclass-brief,' but has been
broken out as a function in its own right for those writing variants on the
`defclass' macro.  If a verbose-style slot specification is given, it is
returned unchanged.

If `automatic-accessors?  is true, an accessor is defined, whether A is
specified or not _unless_ R is specified.  If `automatic-initargs?  is true, 
an initarg is defined whether I is specified or not.  If `conc-name' is
specified, the accessor name has that prepended, with conc-separator, and then 
the slot name. 

All other CLOS slot options are processed normally."
  
  ;; check types
  (etypecase slot
    (symbol (setf slot (list slot)))
    (list nil))
  
  (let* ((name (pop slot))
	 (new-slot (list name))
         (done-initform? nil)
         (done-spec? nil)
         (done-documentation? nil)
         (reader-added? nil)
         (accessor-added? nil)
         (initargs-added? nil))
    (flet ((make-conc-name ()
             (if conc-name
               (intern (format nil "~@:(~A~A~A)" conc-name conc-separator name))
               name))
           
           (add-option (option argument)
             (push option new-slot)
             (push argument new-slot))
           
           ;; Remove duplicate options before returning the slot spec.
           (finish-new-slot (slot)
             ;; XXX This code is overly loopy and opaque ---L
             (destructuring-bind (slot-name &rest options) slot
               (let ((opts (make-hash-table)))
                 (loop for (key val . d) = options then d
                       while key
                       doing (pushnew val (gethash key opts nil) :test #'equal))
                 (loop for key being each hash-key of opts using (hash-value vals)
                       nconc (mapcan #'(lambda (x) (list key x)) vals) into spec
                       finally (return (cons slot-name spec)))))))
      
      (do* ((items slot (rest items))
            (item (first items) (first items))
            (process-item? t t)
            (clos-item? (member item *clos-slot-options*) 
                        (member item *clos-slot-options*)))
           ((null items) nil)
        
        (unless done-initform?
          (setf done-initform? t)
          (unless clos-item?
            (setf  process-item? nil)
            (unless (eq item :UNBOUND)
              (push :initform new-slot)
              (push item new-slot))))
        
        (when process-item?
          (unless (or done-spec? (not (symbolp item)) clos-item?)
            (setf done-spec? t)
            (setf process-item? nil)
            ;; If you've got an A, who cares about R
            (when (find #\A (string item))
              (setf accessor-added? t)
              (add-option :accessor (make-conc-name)))
            (when (and (not accessor-added?) (find #\R (string item)))
              (setf reader-added? t)
              (add-option :reader (make-conc-name)))
            (when (find #\I (string item))
              (setf initargs-added? t)
              (add-option :initarg (intern (string name) (find-package :keyword))))))
        
        (when process-item?
          (unless (or done-documentation? (not (stringp item)))
            (setf done-documentation? t)
            (push :documentation new-slot)
            (push item new-slot)
            ))
        
        (when process-item?
          (when clos-item?
            (push item new-slot)
            (pop items)
            (push (first items) new-slot))))
      
      (when (and automatic-initargs? (not initargs-added?))
        (add-option :initarg (intern (string name) (find-package :keyword))))
      
      (when (and automatic-accessors? (and (not accessor-added?) (not reader-added?)))
        (add-option :accessor (make-conc-name)))
      
      ;; finish-new-slot cleans up duplicates
      (finish-new-slot (nreverse new-slot)))))

;;; ---------------------------------------------------------------------------

(defun convert-clauses-into-lists (clauses-and-options clauses-to-convert)
  ;; This is useful (for me at least!) for writing macros
  (let ((parsed-clauses nil))
    (do* ((clauses clauses-and-options (rest clauses))
          (clause (first clauses) (first clauses)))
         ((null clauses))
      (if (and (keywordp clause)
               (or (null clauses-to-convert) (member clause clauses-to-convert))
               (not (length-1-list-p clauses)))
        (progn
          (setf clauses (rest clauses))
          (push (list clause (first clauses)) parsed-clauses))
        (push clause parsed-clauses)))
    (nreverse parsed-clauses)))

;;; ---------------------------------------------------------------------------

(defun remove-leading-quote (list)
  "Removes the first quote from a list if one is there."
  (if (and (consp list) (eql (first list) 'quote))
    (first (rest list))
    list))

;;; ---------------------------------------------------------------------------

(defun cleanup-parsed-parameter (parameter)
  (if (length-1-list-p parameter)
    (first parameter)
    parameter))


;;; ---------------------------------------------------------------------------
;;; global environment thingies
;;; ---------------------------------------------------------------------------

(defparameter +test-method-prefix+ "%TEST-")
(defvar *current-suite-class-name* nil)
(defvar *current-case-method-name* nil)

(defvar *test-is-being-defined?* nil)
(defvar *test-is-being-compiled?* nil)
(defvar *test-is-being-loaded?* nil)
(defvar *test-is-being-executed?* nil)

(defvar *testsuite-test-count* nil
  "Temporary variable used to 'communicate' between deftestsuite and addtest.")
(defvar *test-output* *debug-io*
  "The destination for output from LIFT. Defaults to *debug-io*.")
(defvar *test-break-on-errors?* nil)
(defvar *test-do-children?* t)
(defparameter *test-ignore-warnings?* nil
  "If true, LIFT will not cause a test to fail if a warning occurs while
the test is running. Note that this may interact oddly with ensure-warning.")
(defparameter *test-print-when-defined?* nil)
(defparameter *test-evaluate-when-defined?* t)
(defparameter *test-scratchpad* nil
  "A place to put things.")

(defparameter *lift-equality-test* 'equal
  "The function used in ensure-same to test if two things are equal. If metatilities is loaded, then you might want to use samep.")

(defvar *test-select-listener-on-problem?* t
  "Not implemented")

(defvar *test-describe-if-not-successful?* t
  "If true, then a complete test description is printed when there are any test warnings or failures. Otherwise, one would need to explicity call describe.")

(defvar *test-print-length* :follow-print
  "The print-length in effect when LIFT prints test results. It works exactly like *print-length* except that it can also take on the value :follow-print. In this case, it will be set to whatever *print-length* is.")
(defvar *test-print-level* :follow-print
  "The print-level in effect when LIFT prints test results. It works exactly like *print-level* except that it can also take on the value :follow-print. In this case, it will be set to whatever *print-level* is.")
          
(defvar *test-environment* nil)


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
  "There is no current test-suite (possibly because ~
   none have been defined yet?). You can specify the ~
   test-suite to test by evaluating (run-tests :suite <suitename>).")

(defparameter +lift-unable-to-parse-test-name-and-class+ 
  "")


;;; ---------------------------------------------------------------------------
;;; test conditions
;;; ---------------------------------------------------------------------------

(define-condition lift-compile-error (error)
                  ((msg :initform "" 
                        :reader msg
                        :initarg :lift-message))
  (:report (lambda (c s)
             (format s "Compile error: '~S'" (msg c)))))

;;; ---------------------------------------------------------------------------

(define-condition test-class-not-defined (lift-compile-error)
                  ((test-class-name :reader test-class-name
                                    :initarg :test-class-name))
  (:report (lambda (c s)
             (format s "Test class ~A not defined before it was used."
                     (test-class-name c)))))

;;; ---------------------------------------------------------------------------

(defun build-lift-error-message (context message &rest args)
  (format nil "~A: ~A" 
          context
          (apply #'format nil message args)))

;;; ---------------------------------------------------------------------------

(defun signal-lift-error (context message &rest args)
  (let ((c (make-condition  
            'lift-compile-error
            :lift-message (apply #'build-lift-error-message context message args))))
    (unless (signal c)
      (error c))))

;;; ---------------------------------------------------------------------------

(defun report-lift-error (context message &rest args)
  (format *test-output* "~&~A."
          (apply #'build-lift-error-message context message args))
  (values))

;;; ---------------------------------------------------------------------------

(defun lift-report-condition (c)
  (format *test-output* "~&~A." c))

;;; ---------------------------------------------------------------------------

(define-condition test-condition (warning) 
                  ((message :initform ""
                            :initarg :message
                            :accessor message))
  (:report (lambda (c s)
             (when (message c)
               (format s "~%~A" (message c))))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-failed-error (test-condition) 
                  ((assertion :initform "" 
                              :accessor assertion
                              :initarg :assertion))
  (:report (lambda (c s)
             (format s "Ensure failed: ~S" (assertion c))
             (call-next-method))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-expected-condition (test-condition) 
                  ((expected-condition-type
                    :initform nil
                    :accessor expected-condition-type
                    :initarg :expected-condition-type)
                   (the-condition
                    :initform nil
                    :accessor the-condition
                    :initarg :the-condition))
  (:report (lambda (c s)
             (format s "Expected ~A but got ~S" 
                     (expected-condition-type c)
                     (the-condition c))
             (call-next-method))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-not-same (test-condition) 
                  ((first-value :accessor first-value
                                :initarg :first-value)
                   (second-value :accessor second-value
                                 :initarg :second-value)
                   (test :accessor test
                         :initarg :test))
  (:report (lambda (c s)
             (format s "Ensure-same: ~S is not ~S to ~S"
                     (first-value c) (test c) (second-value c))
             (call-next-method))))

;;; ---------------------------------------------------------------------------

(defmacro ensure (predicate &key report args)
  `(if ,predicate
     (values t)
     (let ((condition (make-condition 
                       'ensure-failed-error 
                       :assertion ',predicate
                       ,@(when report
                           `(:message (format nil ,report ,@args))))))
       (if (find-restart 'ensure-failed)
         (invoke-restart 'ensure-failed condition) 
         (warn condition)))))

;;; ---------------------------------------------------------------------------

(defmacro ensure-condition (condition &body body)
  "This macro is used to make sure that body really does produce condition."
  (setf condition (remove-leading-quote condition))
  (destructuring-bind (condition &key report args)
                      (if (consp condition) condition (list condition))
    (let ((g (gensym)))
      `(let ((,g nil))
         (unwind-protect
           (handler-case 
             (progn ,@body)
             (,condition (cond) 
                         (declare (ignore cond)) (setf ,g t))
             (condition (cond) 
                        (setf ,g t)
                        (let ((c (make-condition 
                                  'ensure-expected-condition
                                  :expected-condition-type ',condition
                                  :the-condition cond
                                  ,@(when report
                                      `(:message (format nil ,report ,args))))))
                          (if (find-restart 'ensure-failed)
                            (invoke-restart 'ensure-failed c) 
                            (warn c)))))
           (when (not ,g)
             (if (find-restart 'ensure-failed)
               (invoke-restart 'ensure-failed 
                               (make-condition 'ensure-expected-condition
                                               :expected-condition-type ',condition
                                               :the-condition nil
                                               ,@(when report
                                                   `(:message (format nil ,report ,args))))) 
               (warn "Ensure-condition didn't get the condition it expected."))))))))

;;; ---------------------------------------------------------------------------

(defmacro ensure-warning (&body body)
  `(ensure-condition warning ,@body))

;;; ---------------------------------------------------------------------------

(defmacro ensure-error (&body body)
  `(ensure-condition error ,@body))

;;; ---------------------------------------------------------------------------

(defmacro ensure-same (form values &key (test nil test-specified-p) (report nil) (args nil))
  "\(ensure-same value-or-values-1 value-or-values-2
  &key \(test 'equal\) report args\)

Ensure same compares value-or-values-1 value-or-values-2 or each value of value-or-values-1 value-or-values-2 (if they are multiple values) using test. If a problem is encountered ensure-same raises a warning which uses report as a format string and args as arguments to that string (if report and args are supplied). If ensure-same is used within a test, a test failure is generated instead of a warning"
  (setf test (remove-leading-quote test))
  (when (and (consp test)
             (eq (first test) 'function))
    (setf test (second test)))
  `(progn
     (loop for value in (multiple-value-list ,form)
           for other-value in (multiple-value-list ,values) do
           (unless (funcall ',(if test-specified-p test *lift-equality-test*)
                            value other-value)
             (maybe-raise-not-same-condition 
              value other-value
              ',(if test-specified-p test *lift-equality-test*) ,report ,args)))
     (values t)))

;;; ---------------------------------------------------------------------------

(defun maybe-raise-not-same-condition (value-1 value-2 test report args)
  (let ((condition (make-condition 'ensure-not-same 
                                   :first-value value-1
                                   :second-value value-2
                                   :test test
                                   :message (when report
                                              (format nil report args)))))
    (if (find-restart 'ensure-failed)
      (invoke-restart 'ensure-failed condition) 
      (warn condition))))

;;; ---------------------------------------------------------------------------
;;; test-mixin
;;; ---------------------------------------------------------------------------

(defclass test-mixin ()
  ((name :initform nil :initarg :name :accessor name :reader testsuite-name)
   (single-setup? :reader single-setup? :initarg :single-setup?)
   (done-setup? :initform nil :reader done-setup?)
   (prototypes :initform (list (list)) :accessor prototypes)
   (current-values :initform nil :accessor current-values)
   (test-slot-names :initform nil :initarg :test-slot-names :reader test-slot-names)
   (current-step :initform :created :accessor current-step)
   (current-method :initform nil :accessor current-method))
  (:documentation "A simple test suite")
  (:default-initargs
    :single-setup? nil))

;;; ---------------------------------------------------------------------------

(defclass test-result ()
  ((test-class-name :initform nil :initarg :test-class-name :accessor test-class-name)
   (tests-run :initform nil :accessor tests-run)
   (failures :initform nil :accessor failures)
   (errors :initform nil :accessor errors)
   (test-mode :initform :single :initarg :test-mode :accessor test-mode)
   (test-interactive? :initform nil 
                      :initarg :test-interactive? :accessor test-interactive?))
  (:default-initargs
    :test-interactive? *test-is-being-defined?*))

;;; ---------------------------------------------------------------------------

(defgeneric testsuite-setup (test-suite)
  (:documentation "Setup at the testsuite-level")
  (:method ((test-suite test-mixin))
           (values))
  (:method :before ((test-suite test-mixin))
           (setf (current-step test-suite) 'testsuite-setup))
  (:method :after ((test-suite test-mixin))
           (initialize-prototypes test-suite)))

(defgeneric testsuite-run (test-suite result)
  (:documentation "Run the cases in this suite and it's children."))

(defgeneric testsuite-teardown (test-suite)
  (:documentation "Cleanup at the testsuite level.")
  (:method ((test-suite test-mixin))
           (values)))

(defgeneric more-prototypes-p (test-suite)
  (:documentation "Returns true if another prototype set exists for the case."))

(defgeneric initialize-prototypes (test-suite)
  (:documentation "Creates lists of all prototype sets."))

(defgeneric next-prototype (test-suite)
  (:documentation "Ensures that the test environment has the values of the next prototype set."))

(defgeneric setup-test (test-suite)
  (:documentation "Setup for a test-case. By default it does nothing."))

(defgeneric teardown-test (test-suite)
  (:documentation "Tear-down a test-case. By default it does nothing.")
  (:method-combination progn :most-specific-first))

(defgeneric testsuite-methods (test-suite)
  (:documentation "Returns a list of the test methods defined for test. I.e.,
the methods that should be run to do the tests for this test."))

;;; ---------------------------------------------------------------------------

(defmethod setup-test ((test test-mixin))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod teardown-test progn ((test test-mixin))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod initialize-test ((test test-mixin))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod initialize-test :before ((test test-mixin))
  (next-prototype test))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((tc test-mixin) &key)
  (when (null (testsuite-name tc))
    (setf (slot-value tc 'name) (symbol-name (type-of tc)))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((tc test-mixin) stream)
  (format stream "#<TEST SUITE: ~A>"
          (testsuite-name tc)))

;;; ---------------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------------

(defvar *current-definition* nil
  "An associative-container which saves interesting information about
the thing being defined.")

;;; ---------------------------------------------------------------------------

(defun initialize-current-definition ()
  (setf *current-definition* nil))

;;; ---------------------------------------------------------------------------

(defun set-definition (name value)
  (let ((current (assoc name *current-definition*)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *current-definition*)))
  
  (values value))

;;; ---------------------------------------------------------------------------

(defun def (name &optional (definition *current-definition*))
  (when definition (cdr (assoc name definition))))

;;; ---------------------------------------------------------------------------

(defun (setf def) (value name)
  (set-definition name value))

;;; ---------------------------------------------------------------------------

(defvar *code-blocks* nil)

;;; ---------------------------------------------------------------------------

(defstruct (code-block (:type list) (:conc-name nil))
  block-name (priority 0) filter code operate-when)

;;; ---------------------------------------------------------------------------

(defgeneric block-handler (name value)
  (:documentation "")
  (:method ((name t) (value t))
           (error "Unknown clause: ~A" name)))

;;; ---------------------------------------------------------------------------

(defun add-code-block (name priority operate-when filter handler code)
  (let ((current (assoc name *code-blocks*))
        (value (make-code-block
                :operate-when operate-when
                :block-name name
                :priority priority
                :filter filter
                :code code)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *code-blocks*))
  
    (eval 
     `(defmethod block-handler ((name (eql ',name)) value)
        (declare (ignorable value))
        ,@handler)))
    
    (setf *code-blocks* (sort *code-blocks* #'< :key (lambda (name.cb)
                                                       (priority (cdr name.cb))))))

;;; ---------------------------------------------------------------------------

(defmacro with-test-slots (&body body)
  `(symbol-macrolet
     ,(mapcar #'(lambda (local)
                  `(,local (test-environment-value ',local)))
              (def :slot-names))
     ,@body))

;;; ---------------------------------------------------------------------------

(defvar *deftest-clauses*
  '(:setup :teardown :test :documentation :tests :export-p :export-slots
    :single-setup))

;;; ---------------------------------------------------------------------------

(defmacro deftest (testsuite-name superclasses slots &rest
                                  clauses-and-options) 
  (warn "Deftest is obsolete, use deftestsuite instead.")
  `(deftestsuite ,testsuite-name ,superclasses ,slots ,@clauses-and-options))

;;; ---------------------------------------------------------------------------

(setf *code-blocks* nil)

(add-code-block
 :setup 1 :methods
 (lambda () (def :setup)) 
 '((setf (def :setup) (cleanup-parsed-parameter value)))
 'build-setup-test-method)

(add-code-block
 :teardown 100 :methods
 (lambda () (or (def :teardown) (def :direct-slot-names))) 
 '((setf (def :teardown) (cleanup-parsed-parameter value)))
 'build-test-teardown-method)

(add-code-block
 :documentation 0 :class-def 
 nil 
 '((setf (def :documentation) (first value)))
 nil)

(add-code-block
 :export-p 0 :class-def
 nil 
 '((setf (def :export-p) (first value)))
 nil)

(add-code-block
 :export-slots 0 :class-def
 nil 
 '((setf (def :export-slots) (first value)))
 nil)

(add-code-block
 :single-setup 0 :class-def
 nil 
 '((setf (def :single-setup) (first value)))
 nil)

(defmacro deftestsuite (testsuite-name superclasses slots &rest
                                  clauses-and-options) 
  "DEFTEST testsuite-name
\({superclass-name}*\) \({slot-name | (slot-name
[slot-option*])}*\) [(test-option)]*

Creates a test-suite named 'testsuite-name' and, optionally,
the code required for test setup, test tear-down and the
actual test-cases. A test-suite is a collection of
test-cases and other test-suites.

Test-options are :setup, :teardown, :test, :tests,
:documentation, and :export-p"
  
  #+NO-LIFT-TESTS
  `(values)
  
  #-NO-LIFT-TESTS
  (let ((test-list nil)
        (options nil)
        (return (gensym)))
    
    ;; convert any clause like :setup foo into (:setup foo)
    (setf clauses-and-options 
          (convert-clauses-into-lists clauses-and-options *deftest-clauses*))
    
    (initialize-current-definition)
    (setf (def :testsuite-name) testsuite-name)
    (setf (def :superclasses) superclasses)
    (setf (def :deftestsuite) t)
    
    ;; parse clauses into defs
    (loop for clause in clauses-and-options do
          (typecase clause
            (symbol (pushnew clause options))
            (cons (destructuring-bind (kind &rest spec) clause
                    (case kind
                      (:test (push (first spec) test-list))
                      (:tests 
                       (loop for test in spec do
                             (push test test-list)))
                      (t (block-handler kind spec)))))
            (t (error "When parsing ~S" clause))))
    
    (let ((slot-names nil) (slot-specs nil))
      (loop for slot in (if (listp slots) slots (list slots)) do 
            (push (if (consp slot) (first slot) slot) slot-names)
            (push (parse-brief-slot slot nil nil nil nil) slot-specs))
      (setf (def :slot-specs) (nreverse slot-specs)
            (def :direct-slot-names) (nreverse slot-names)
            (def :slots-parsed) t))
    
    (setf (def :slot-names) (compute-all-slots))
    
    (empty-test-tables testsuite-name)                 
    (prog2
     (setf *testsuite-test-count* 0)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (eval-when (:compile-toplevel)
          (push ',return *test-is-being-compiled?*))
        (eval-when (:load-toplevel)
          (push ',return *test-is-being-loaded?*))
        (eval-when (:execute)
          (push ',return *test-is-being-executed?*))
        (unwind-protect
          (let ((*test-is-being-defined?* t))
            (handler-case 
              (progn
                ;; remove previous methods (do this _before_ we define the class)
                (remove-previous-definitions ',(def :testsuite-name))
                (setf *current-case-method-name* nil)
                
                ;; and then redefine the class
                ,(build-test-class)
                (setf *current-suite-class-name* ',(def :testsuite-name))
                
                ,@(when (def :export-p)
                    `((export '(,(def :testsuite-name)))))
                ,@(when (def :export-slots?)
                    `((export ',(def :direct-slot-names))))
                
                ;; make a place to save test-case information
                (empty-test-tables ',(def :testsuite-name))
                
                ;;; create methods
                ;; setup :before
                ,@(build-initialize-test-method) 
                
                ,@(loop for (nil . block) in *code-blocks* 
                        when (and block 
                                  (code block)
                                  (eq (operate-when block) :methods)
                                  (or (not (filter block))
                                      (funcall (filter block)))) collect
                        (funcall (code block)))
                
                ;; tests
                ,@(when test-list
                    `((let ((*test-evaluate-when-defined?* nil))
                        ,@(loop for test in (nreverse test-list) collect
                                `(addtest (,(def :testsuite-name)) 
                                   ,@test))
                        (setf *testsuite-test-count* nil))))
                
                ,(if *test-evaluate-when-defined?* 
                   `(unless (or *test-is-being-compiled?*
                                *test-is-being-loaded?*)
                      (let ((*test-break-on-errors?* *test-break-on-errors?* 
                             #+Ignore
                             ;;?? Gary King 2006-01-23: what was I thinking?
                             *test-is-being-defined?*))
                        (run-tests :suite ',testsuite-name)))
                   `(find-class ',testsuite-name)))
              
              (condition (c) 
                         (setf *testsuite-test-count* nil)
                         (lift-report-condition c))))
          
          ;; cleanup
          (setf *test-is-being-compiled?* (remove ',return *test-is-being-compiled?*))
          (setf *test-is-being-loaded?* (remove ',return *test-is-being-loaded?*))
          (setf *test-is-being-executed?* (remove ',return *test-is-being-executed?*)))))))

;;; ---------------------------------------------------------------------------
  
(defun compute-all-slots ()
  (let ((done nil)
        (slots nil))
    (dolist (super (def :superclasses))
      (if (find-class super nil)
        (dolist (class (superclasses super))
          (when (and (test-suite-p class) 
                     (not (member class done)))
            (push class done)
            (setf slots (append slots (test-slot-names (make-instance class))))))
        (error 'test-class-not-defined :test-class-name super)))
    (remove-duplicates (append (def :direct-slot-names) slots)))) 

;;; ---------------------------------------------------------------------------

(defmacro addtest (name &body test)
  "Adds a single new test-case to the most recently defined test-suite."
  #+NO-LIFT-TESTS
  `(values)
  
  #-NO-LIFT-TESTS
  (handler-case 
    (let ((body nil)
          (return (gensym)))
      (cond ((length-1-list-p name)
             ;; testsuite given
             (setf (def :testsuite-name) (first name) name nil body test))
            (t
             ;; the 'name' is really part of the test...
             (setf body (cons name test))))
      
      (unless (def :testsuite-name)
        (when *current-suite-class-name*
          (setf (def :testsuite-name) *current-suite-class-name*)))
      
      (unless (def :testsuite-name)
        (signal-lift-error 'add-test +lift-no-current-test-class+))
      (unless (or (def :deftestsuite) 
                  (find-class (def :testsuite-name) nil))
        (signal-lift-error 'add-test +lift-test-class-not-found+
                           (def :testsuite-name)))
      
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (eval-when (:compile-toplevel)
           (push ',return *test-is-being-compiled?*))
         (eval-when (:load-toplevel)
           (push ',return *test-is-being-loaded?*))
         (eval-when (:execute)
           (push ',return *test-is-being-executed?*))
         
         (unwind-protect
           (let ((*test-is-being-defined?* t))
             ,(build-test-test-method (def :testsuite-name) body)
             (setf *current-suite-class-name* ',(def :testsuite-name))
             (if *test-evaluate-when-defined?*
               (unless (or *test-is-being-compiled?*
                           *test-is-being-loaded?*)
                 (let ((*test-break-on-errors?* (testing-interactively-p)))
                   (run-test)))
               (values)))
           
           ;; cleanup
           (setf *test-is-being-compiled?* (remove ',return *test-is-being-compiled?*))
           (setf *test-is-being-loaded?* (remove ',return *test-is-being-loaded?*))
           (setf *test-is-being-executed?* (remove ',return *test-is-being-executed?*)))))
    (condition (c) 
               (lift-report-condition c))))

;;; ---------------------------------------------------------------------------

(defun remove-test (&key (name *current-case-method-name*)
                         (suite *current-suite-class-name*))
  (assert suite nil "Test suite could not be determined.")
  (assert name nil "Test name could not be determined.")
  
  (handler-case 
    (let ((test-suite (make-instance suite))) 
      (multiple-value-bind (m gf)
                           (testsuite-name->method test-suite name)
        (unless (and gf m)
          (warn +lift-could-not-find-test+ suite name))
        (format *test-output* "~&Removing test: ~(~A.~A~)." suite name)
        (remove-method gf m)))
    (condition (c) 
               (lift-report-condition c))))

;;; ---------------------------------------------------------------------------

(defun run-test (&key (name *current-case-method-name*)
                      (suite *current-suite-class-name*) 
                      (break-on-errors? *test-break-on-errors?*)
                      (do-children? *test-do-children?*)
                      (result nil))
  (assert suite nil "Test suite could not be determined.")
  (assert name nil "Test name could not be determined.")
  
  (let* ((*test-break-on-errors?* break-on-errors?)
         (*test-do-children?* do-children?)
         (test-suite (make-instance suite))
         (gf (nth-value 1 (testsuite-name->method test-suite name))))
    (unless result
      (setf result (make-test-result suite :single)))
    (setf *current-case-method-name* name
          *current-suite-class-name* suite)
    (do-testing test-suite result
                (lambda () 
                  (run-test-internal test-suite gf result)))))

;;; ---------------------------------------------------------------------------

(defmethod do-testing ((test-suite test-mixin) result fn)
  (unwind-protect
    (progn
      (testsuite-setup test-suite)
      (do ()
          ((not (more-prototypes-p test-suite)) result)
        (initialize-test test-suite) 
        (funcall fn)))
    ;; cleanup
    (testsuite-teardown test-suite))
  
  (values result))

;;; ---------------------------------------------------------------------------

(defmethod run-tests-internal ((case symbol) &rest args &key &allow-other-keys)
  (apply #'run-tests-internal (make-instance case) args))

;;; ---------------------------------------------------------------------------

(defmethod run-tests-internal ((test-class standard-class) &rest args &key &allow-other-keys)
  (apply #'run-tests-internal (class-name test-class) args))

;;; ---------------------------------------------------------------------------

(defmethod run-tests-internal ((case test-mixin) &key 
                               (result (make-test-result (class-of case) :multiple)))
  (do-testing case result 
              (lambda ()
                (testsuite-run case result))))

;;; ---------------------------------------------------------------------------

#+Later
(defmacro with-test (&body forms)
  "Execute forms in the context of the current test class."
  (let* ((test-class-name *current-suite-class-name*)
         (test-case (make-instance test-class)))
    `(eval-when (:execute)
       (prog2
        (setup-test ,test-case)
        (progn
          (with-test-slots ,@forms))
        (teardown-test ,test-case)))))

;;; ---------------------------------------------------------------------------

(defun testsuites (&optional (start-at 'test-mixin))
  "Returns a list of testsuite classes. The optional parameter provides
control over where in the test hierarchy the search begins."
  (let* ((class (find-class start-at nil))
         (subclasses (and class (subclasses class :proper? t))))
    (loop for subclass in subclasses
          when (subtypep subclass 'test-mixin) 
          collect (class-name subclass))))

;;; ---------------------------------------------------------------------------

(defgeneric testsuite-tests (suite)
  (:documentation "Returns a list of test names instances for a suite.")
  (:method ((suite symbol))
           (testsuite-tests (make-instance suite)))
  (:method ((suite standard-class))
           (testsuite-tests (make-instance (class-name suite))))
  (:method ((suite test-mixin))
           (nreverse
            (mapcar (lambda (method)
                      (intern (format nil "~@:(~A~)" (testsuite-method->name method))))
                    (testsuite-methods suite))))) 

;;; ---------------------------------------------------------------------------

(defun list-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Lists all of the defined test classes from :start-at on down."
  (mapc (lambda (subclass-name)
          (format stream "~&~A (~D)" 
                  subclass-name
                  (length (testsuite-methods subclass-name)))
          (when include-cases?
            (loop for method-name in (testsuite-tests subclass-name) do
                  (format stream "~&  ~A" method-name))))
        (testsuites start-at))
  (values))

;;; ---------------------------------------------------------------------------

(defun testsuite-test-count (testsuite)
  (or (and *testsuite-test-count* 
           (prog1 *testsuite-test-count* (incf *testsuite-test-count*))) 
      (length (testsuite-methods testsuite))))

;;; ---------------------------------------------------------------------------

(defun run-tests (&rest args &key 
                        (suite *current-suite-class-name*) &allow-other-keys)
  "Run all of the tests in a suite. Arguments are :suite, :result, :do-children? and :break-on-errors?" 
  (remf args :suite)
  (cond ((null suite)
         (progn
           (report-lift-error 'run-tests +run-tests-null-test-case+)
           (values)))
        (t
         (setf *current-suite-class-name* suite)
         (apply #'run-tests-internal suite args))))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-run ((case test-mixin) (result test-result))
  (let ((methods (testsuite-methods case)))
    (loop for method in methods do
          (run-test-internal case method result))
    (when *test-do-children?*
      (loop for subclass in (direct-subclasses (class-of case))
            when (test-suite-p subclass) do
            (run-tests-internal (make-instance (class-name subclass)) 
                                :result result)))))

;;; ---------------------------------------------------------------------------

(defmethod more-prototypes-p ((test-suite test-mixin))
  (not (null (prototypes test-suite))))

;;; ---------------------------------------------------------------------------

(defmethod initialize-prototypes ((test-suite test-mixin))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod next-prototype ((test-suite test-mixin))
  (setf (current-values test-suite) (first (prototypes test-suite)) 
        (prototypes test-suite) (rest (prototypes test-suite)))
    (dolist (key.value (current-values test-suite))
      (setf (test-environment-value (car key.value)) (cdr key.value))))

;;; ---------------------------------------------------------------------------

(defmethod run-test-internal ((case test-mixin) (method standard-generic-function) 
                              result)
  (let ((name (testsuite-method->name method))
        (problem nil))
    (tagbody 
      :test-start
      (restart-case
        (handler-bind ((warning #'muffle-warning)       ; Decline to handle warnings... 
                       (error 
                        (lambda (cond)
                          (setf problem 
                                (report-test-problem 'test-error result case method cond))
                          (if *test-break-on-errors?*
                            (break)
                            (go :test-end))))
                       (t (lambda (cond)
                            (setf problem 
                                  (report-test-problem 'test-error result case method cond)))))
          (setf problem nil)
          (start-test result case name)
          (setup-test case)
          (unwind-protect
            (progn
              (setf (current-step case) :testing
                    (current-method case) method)
              (funcall method case))
            (teardown-test case)
            (end-test result case name)))
        (ensure-failed (cond) 
                       (setf problem 
                             (report-test-problem 'test-failure result case method cond)))
        (retry-test () :report "Retry the test." 
                    (go :test-start)))
      :test-end))

  result)

;;; ---------------------------------------------------------------------------

(defun report-test-problem (problem-type result suite method condition)
  (let ((problem (make-instance problem-type
                   :test-suite suite
                   :test-method method 
                   :test-condition condition
                   :test-step (current-step suite))))
    (setf (first (tests-run result))
          (append (first (tests-run result)) (list problem))) 
    (etypecase problem
      (test-failure (push problem (failures result)))
      (test-error (push problem (errors result))))
    problem))

;;; ---------------------------------------------------------------------------
;;; test-result and printing
;;; ---------------------------------------------------------------------------

(defun get-test-print-length ()
  (let ((foo *test-print-length*))
    (if (eq foo :follow-print) *print-length* foo)))

;;; ---------------------------------------------------------------------------

(defun get-test-print-level ()
  (let ((foo *test-print-level*))
    (if (eq foo :follow-print) *print-level* foo)))

;;; ---------------------------------------------------------------------------

(defmethod start-test ((result test-result) (case test-mixin) method-name) 
  (push (list method-name (current-values case)) (tests-run result)))

;;; ---------------------------------------------------------------------------

(defmethod end-test ((result test-result) (case test-mixin) method-name)
  (declare (ignore method-name)))

;;; ---------------------------------------------------------------------------

(defmethod make-test-result ((test-class-name symbol) test-mode)
  (make-instance 'test-result
    :test-class-name test-class-name
    :test-mode test-mode))

;;; ---------------------------------------------------------------------------

(defmethod make-test-result ((test-class standard-class) test-mode)
  (make-test-result (class-name test-class) test-mode))

;;; ---------------------------------------------------------------------------

(defun testing-interactively-p ()
  (values nil)
  #+Ignore
  (and *test-is-being-defined?*
       (not (or *test-is-being-compiled?*
                ))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((tr test-result) stream)
  (let ((complete-success? (and (null (errors tr))
                                (null (failures tr))))) 
    (let* ((*print-level* (get-test-print-level))
           (*print-length* (get-test-print-length)))
      (print-unreadable-object (tr stream)
        (cond ((null (tests-run tr))
               (format stream "~A: no tests defined" (test-class-name tr)))
              
              ((eq (test-mode tr) :single)
               (cond ((test-interactive? tr)
                      ;; interactive
                      (cond (complete-success?
                             (format stream "Test passed"))
                            ((errors tr)
                             (format stream "Error during testing"))
                            (t
                             (format stream "Test failed"))))
                     
                     (t
                      ;; from run-test
                      (format stream "~A.~A ~A" 
                              (test-class-name tr) 
                              (first (first (tests-run tr)))
                              (cond (complete-success?
                                     "passed")
                                    ((errors tr)
                                     "Error")
                                    (t
                                     "failed"))))))
              
              (t
               ;; multiple tests run
               (format stream "Results for ~A " (test-class-name tr))
               (if complete-success?
                 (format stream "[~A Successful test~:P]"
                         (length (tests-run tr)))
                 (format stream "~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]." 
                         (length (tests-run tr))
                         (length (failures tr))
                         (length (errors tr))))))
        
        ;; not that suites with no tests think that they are completely successful
        ;; optimisitc little buggers, huh?
        (when (and (not complete-success?) *test-describe-if-not-successful?*)
          (format stream "~%") 
          (print-test-result-details stream tr))))))

;;; ---------------------------------------------------------------------------

(defmethod describe-object ((result test-result) stream)
  (let ((number-of-failures (length (failures result)))
        (number-of-errors (length (errors result))))
    
    (unless *test-is-being-defined?*
      (format stream "~&Test Report for ~A: ~D test~:P run" 
              (test-class-name result) (length (tests-run result))))
    (let* ((*print-level* (get-test-print-level))
           (*print-length* (get-test-print-length)))
      (cond ((or (failures result) (errors result))
             (format stream "~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]." 
                     number-of-failures
                     number-of-errors)
             (format stream "~%~%")
             
             (print-test-result-details stream result))
            (t
             (unless *test-is-being-defined?*
               (format stream ", all passed!")))))
    
    (values)))

;;; ---------------------------------------------------------------------------

(defun print-test-result-details (stream result)
  (dolist (report (failures result))
    (print-test-problem "Failure: " report stream))
  
  (dolist (report (errors result))
    (print-test-problem "ERROR  : " report stream)))

;;; ---------------------------------------------------------------------------

(defun print-test-problem (prefix report stream)
  (let* ((suite (test-suite report))
         (method (test-method report))
         (condition (test-condition report))
         (code (test-report-code suite method))
         (testsuite-name (testsuite-method->name method)))
    
    (format stream "~&~A~(~A : ~A~)" prefix (type-of suite) testsuite-name)
    (let ((doc-string (gethash (intern testsuite-name) 
                               (test-case-documentation (class-name (class-of suite))))))
      (when doc-string 
        (format stream "~&~A" doc-string)))
    
    (format stream "~&~<  ~@;~
                    ~@[Condition: ~<~@;~A~:>~]~
                    ~@[~&Code     : ~S~]~
                    ~&~:>" (list (list condition) code))))


;;; ---------------------------------------------------------------------------
;;; test-reports
;;; ---------------------------------------------------------------------------

(defclass test-problem-mixin ()
  ((test-suite :initform nil :initarg :test-suite :reader test-suite)
   (test-method :initform nil :initarg :test-method :reader test-method)
   (test-condition :initform nil :initarg :test-condition :reader test-condition)
   (test-problem-kind :reader test-problem-kind :allocation :class)
   (test-step :initform nil :initarg :test-step :reader test-step)))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((problem test-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "TEST-~@:(~A~): ~A in ~A" 
            (test-problem-kind problem) 
            (name (test-suite problem))
            (etypecase (test-method problem)
              (function (testsuite-method->name (test-method problem)))
              (symbol (test-method problem))))))

;;; ---------------------------------------------------------------------------

(defclass test-failure (test-problem-mixin)
  ((test-problem-kind :initform "Failure" :allocation :class)))

;;; ---------------------------------------------------------------------------

(defclass test-error (test-problem-mixin)
  ((test-problem-kind :initform "ERROR" :allocation :class)))

;;; ---------------------------------------------------------------------------

(defmethod test-report-code ((test-suite test-mixin) (method standard-generic-function))
  (let* ((class-name (class-name (class-of test-suite))))
    (gethash (intern (concatenate 'string +test-method-prefix+ 
                                  (testsuite-method->name method))) 
             (test-name->code-table class-name))))

;;; ---------------------------------------------------------------------------

(defmethod test-report-code ((test-suite test-mixin) (method symbol))
  "Test code unavailable")


;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun remove-previous-definitions (classname)
  "Remove the methods of this class and all its subclasses."
  (let ((classes-removed nil)
        (class (find-class classname nil))
        (removed-count 0))
    (when class
      (loop for subclass in (subclasses class :proper? nil) do
            (push subclass classes-removed)
            (incf removed-count
                  (remove-methods (class-name subclass) :verbose? nil))
            #+Ignore
            ;;?? causing more trouble than it solves...??
            (setf (find-class (class-name subclass)) nil))
      
      (unless (length-1-list-p classes-removed)
        (format *test-output* 
                "~&;;; Removed Test suite ~(~A~) and its subclasses (~{~<~s~>~^, ~})."
                classname (sort 
                           (delete classname (mapcar #'class-name classes-removed))
                           #'string-lessp)))
      (unless (zerop removed-count)
        (format *test-output* 
                "~&;;; Removed ~D methods from test suite ~(~A~)~@[ and its subclasses~]."
                removed-count classname (not (length-1-list-p classes-removed)))))))

;;; ---------------------------------------------------------------------------

(defun build-initialize-test-method ()
  (let ((initforms nil)
        (slot-names nil)
        (slot-specs (def :slot-specs)))
    (loop for slot in slot-specs do
          (when (and (member :initform (rest slot))
                     (not (eq :unbound (getf (rest slot) :initform))))
            (push (getf (rest slot) :initform) initforms)
            (push (first slot) slot-names)))
    (setf slot-names (nreverse slot-names)
          initforms (nreverse initforms))
    
    (when initforms
      `((defmethod initialize-prototypes :after ((test ,(def :testsuite-name)))
          (with-test-slots
            (setf (prototypes test)
                  (let* (,@(mapcar (lambda (slot-name initform)
                                     `(,slot-name ,initform))
                                   slot-names initforms))
                    (list
                     (list ,@(mapcar (lambda (slot-name)
                                       `(cons ',slot-name ,slot-name))
                                     slot-names)))))))))))

;;; ---------------------------------------------------------------------------

(defun (setf test-environment-value) (value name)
  (pushnew (cons name value) *test-environment* :test #'equal)
  (values value))

;;; ---------------------------------------------------------------------------

(defun test-environment-value (name)
  (cdr (assoc name *test-environment*)))

;;; ---------------------------------------------------------------------------

(defun remove-from-test-environment (name)
  (setf *test-environment* 
        (remove name *test-environment* :key #'car)))

;;; ---------------------------------------------------------------------------

(defun build-test-teardown-method ()
  (let ((test-name (def :testsuite-name))
        (slot-names (def :direct-slot-names))
        (teardown (def :teardown))
        (single-setup? (def :single-setup?)))
    
    (when teardown
      (unless (consp teardown)
        (setf teardown (list teardown)))
      (when (length-1-list-p teardown)
        (setf teardown (list teardown)))
      (when (symbolp (first teardown))
        (setf teardown (list teardown))))
    (let* ((teardown-code `(,@(when teardown
                                `((with-test-slots ,@teardown)))))
           
           (test-code `(,@(unless single-setup? teardown-code)
                        ,@(mapcar (lambda (slot)
                                    `(remove-from-test-environment ',slot))
                                  slot-names)))
           (suite-code `(,@(when single-setup? (list teardown-code)))))
      `(progn
         ,@(when test-code
             `((defmethod teardown-test progn ((test ,test-name))
                 ,@test-code)))
         ,@(when suite-code
             `((defmethod testsuite-teardown progn ((test ,test-name))
                 ,@test-code)))))))

;;; ---------------------------------------------------------------------------

(defun build-setup-test-method ()
  (let ((test-name (def :testsuite-name))
        (setup (def :setup))
        (single-setup? (def :single-setup?)))
    (when setup
      (unless (consp setup)
        (setf setup (list setup)))
      (when (length-1-list-p setup)
        (setf setup (list setup)))
      (when (symbolp (first setup))
        (setf setup (list setup))))
    (let ((code `((with-test-slots ,@setup))))
      `(progn
         (defmethod setup-test :after ((test ,test-name))
           ,@(if single-setup?
               `((unless (done-setup? test)
                   (setf (slot-value test 'done-setup?) t)
                   ,@code))
               `,code))))))

;;; ---------------------------------------------------------------------------

(defun build-test-test-method (test-class test-body)
  (multiple-value-bind (test-name body documentation name-supplied?)
                       (parse-test-body test-body)
    (unless (consp (first body))
      (setf body (list body)))
    
    `(progn
       (setf (gethash ',test-name (test-name->code-table ',test-class)) ',body
             (gethash ',body (test-code->name-table ',test-class)) ',test-name)
       
       ,(when documentation
          `(setf (gethash ',test-name (test-case-documentation ',test-class))
                 ,documentation))

       #+MCL
       ,@(when name-supplied?
           `((let ((ccl:*warn-if-redefine* nil))
               (ccl:record-source-file ',test-name 'test-case))))
       
       (defmethod ,test-name ((test ,test-class))
         (with-test-slots ,@body))
       (setf *current-case-method-name* 
             (intern (method-name->test-name (symbol-name ',test-name))))
       
       (when (and *test-print-when-defined?*
                  (not (or *test-is-being-compiled?*
                           )))
         (format *test-output* "~&;Test Created: ~(~S.~S~)." ',test-class ',test-name))
       *current-case-method-name*)))

;;; ---------------------------------------------------------------------------

(defun parse-test-body (test-body)
  (let ((test-name nil)
        (body nil)
        (parsed-body nil)
        (documentation nil)
        (test-number (1+ (testsuite-test-count *current-suite-class-name*)))
        (name-supplied? nil))
    
    ;; parse out any documentation
    (loop for form in test-body do
          (if (and (consp form)
                   (keywordp (first form))
                   (eq :documentation (first form)))
            (setf documentation (second form))
            (push form parsed-body)))
    (setf test-body (nreverse parsed-body))
    (setf test-name (first test-body))
    
    (cond ((symbolp test-name)
           (setf test-name (intern (format nil "~A~A" +test-method-prefix+ test-name))
                 body (rest test-body)
                 name-supplied? t))
          ((and (test-code->name-table *current-suite-class-name*)
                (setf test-name 
                 (gethash test-body (test-code->name-table *current-suite-class-name*))))
           (setf body test-body))
          (t
           (setf test-name (intern (format nil "~ATEST-~A" +test-method-prefix+ test-number))
                 body test-body)))
    
    (values test-name body documentation name-supplied?)))

;;; ---------------------------------------------------------------------------

(defun build-test-class ()
  (let ((single-setup? (def :single-setup)))
    
    ;; for now, we don't generate code from :class-def code-blocks
    ;; they are executed only for effect.
    (loop for (nil . block) in *code-blocks* 
          when (and block 
                    (code block)
                    (eq (operate-when block) :class-def)
                    (or (not (filter block))
                        (funcall (filter block)))) collect
          (funcall (code block)))
    
    (unless (some (lambda (superclass)
                    (test-suite-p superclass))
                  (def :superclasses))
      (pushnew 'test-mixin (def :superclasses)))
    
    ;; build basic class and standard class
    `(progn
       #+MCL
       (ccl:record-source-file ',(def :testsuite-name) 'test-suite)
       (let (#+MCL (ccl:*record-source-file* nil))
         (defclass ,(def :testsuite-name) (,@(def :superclasses))
           nil
           ,@(when (def :documentation)
               `((:documentation ,(def :documentation))))
           (:default-initargs
             :single-setup? ',(remove-leading-quote single-setup?)
             :test-slot-names ',(def :slot-names)
             ,@(def :default-initargs)))))))

;;; ---------------------------------------------------------------------------

(defun parse-test-slots (slot-specs)
  (loop for spec in slot-specs collect
        (let ((parsed-spec spec))
          (if (member :initform parsed-spec)
            (let ((pos (position :initform parsed-spec)))
              (append (subseq parsed-spec 0 pos)
                      (subseq parsed-spec (+ pos 2))))
            parsed-spec))))

;;; ---------------------------------------------------------------------------

(defmethod test-suite-p ((classname symbol))
  (let ((class (find-class classname nil)))
    (handler-case
      (and class
           (typep (allocate-instance class) 'test-mixin))
      (error (c) (declare (ignore c)) (values nil)))))

;;; ---------------------------------------------------------------------------

(defmethod test-suite-p ((object standard-object))
  (test-suite-p (class-name (class-of object))))

;;; ---------------------------------------------------------------------------

(defmethod test-suite-p ((class standard-class))
  (test-suite-p (class-name class)))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-methods ((classname symbol))
  (remove-if-not
   (lambda (gf)
     (let ((name (generic-function-name gf)))
       (etypecase name
         (symbol (test-method-name-p (symbol-name name)))
         (cons nil))))
   (generic-functions classname)))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-methods ((test test-mixin))
  (testsuite-methods (class-name (class-of test))))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-methods ((test standard-class))
  (testsuite-methods (class-name test)))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-method->name ((method t))
  (testsuite-method->name (method-generic-function method)))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-method->name ((gf standard-generic-function))
  (method-name->test-name (symbol-name (generic-function-name gf))))

;;; ---------------------------------------------------------------------------

(defun method-name->test-name (test-name)
  (subseq test-name (length +test-method-prefix+)))

;;; ---------------------------------------------------------------------------

(defun test-method-name-p (test-name)
  (string-equal +test-method-prefix+ test-name
                :end2 (min (length +test-method-prefix+) (length test-name))))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-method->name ((gf standard-method))
  (let ((method (method-generic-function gf)))
    (when method
      (testsuite-method->name method))))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-name->gf ((case test-mixin) name)
  ;; returns the gf implementing the name for the test-case
  (find (symbol-name name)
        (testsuite-methods case)
        :key (lambda (method)
               (testsuite-method->name method))
        :test #'string-equal))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-name->method ((case test-mixin) name)
  (let ((gf (testsuite-name->gf case name))
        (class (class-of case)))
    (when gf
      (loop for m in (generic-function-methods gf) do
            (when (member class (method-specializers m))
              (return-from testsuite-name->method (values m gf)))))))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-name->method ((object standard-class) name)
  (testsuite-name->method (allocate-instance object) name))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-name->method ((class symbol) (name symbol))
  (testsuite-name->method (allocate-instance (find-class class)) name))

;;; ---------------------------------------------------------------------------

;; some handy properties
(defclass-property test-slots)
(defclass-property test-code->name-table)
(defclass-property test-name->code-table)
(defclass-property test-case-documentation)
(defclass-property testsuite-prototype)

;;; ---------------------------------------------------------------------------

(defun empty-test-tables (test-name)
  (when (find-class test-name nil)
    (setf (test-code->name-table test-name)
          (make-hash-table :test #'equal)
          (test-name->code-table test-name)
          (make-hash-table :test #'equal)
          (test-case-documentation test-name)
          (make-hash-table :test #'equal))))





