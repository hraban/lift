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

(in-package lift)

(pushnew :cases *deftest-clauses*)

(add-code-block
 :cases 2 :methods
 (lambda () (def :cases)) 
 '((setf (def :cases) (cleanup-parsed-parameter value)))
 'build-cases-method)

;;; ---------------------------------------------------------------------------

(defun build-cases-method ()
  (when (atom (car (def :cases)))
    (setf (def :cases) (list (def :cases))))
  ;(spy (def :cases))
  (let ((cases (standardize-cases-form (def :cases))))
    `(defmethod initialize-prototypes :after ((test ,(def :testsuite-name)))
       (setf (prototypes test) 
             (rest (process-cases-form ,(first cases) 
                                       ,@(mapcar (lambda (a) `',a) (rest cases))))))))

;;; ---------------------------------------------------------------------------

;; goal is spec := (<tag> <spec>+)
;;         spec := (<var value>+)
(defun standardize-cases-form (cases)
  (cond ((atom (first cases))
         (cond ((valid-tag-p (first cases)) 
                `(,(first cases) ,@(mapcar #'standardize-cases-form (rest cases))))
               (t
                cases)))
        ((and (length-1-list-p cases)
              (consp (first cases))
              (valid-tag-p (first (first cases))))
         (standardize-cases-form (first cases)))
        (t
         `(:cross ,@(mapcar #'standardize-cases-form cases)))))

;;; ---------------------------------------------------------------------------

(defun check-subcases (cases)
  (cond ((not (valid-tag-p (first cases)))
         `(,(default-cases-tag) ,@(mapcar #'standardize-cases-form cases)))
        (t
         (mapcar #'standardize-cases-form cases))))

;;; ---------------------------------------------------------------------------

(defun default-cases-tag ()
  :cross)

;;; ---------------------------------------------------------------------------

(defun valid-tag-p (tag)
  (member tag '(:map :cross)))

;;; ---------------------------------------------------------------------------

(defmethod process-cases-form :around ((type t) &rest forms)
  (apply #'call-next-method type (if (atom (car forms))
                                   (list forms) forms)))

;;; ---------------------------------------------------------------------------
  
(defmethod process-cases-form ((type t) &rest forms)
  (cond ((atom (first type))
         (apply #'process-cases-form (first type) (append (rest type) forms)))
        (t (apply #'process-cases-form :cross (append type forms)))))

#+Old
(defmethod process-cases-form ((type (eql :map)) &rest forms)
  (let ((vars (mapcar #'car forms))
        (values (mapcar #'rest forms)))
    `(let (,@(mapcar (lambda (var value) `(,var ,@value))
                     vars values))
       (mapcar (lambda ,vars
                 (list ,@(mapcar (lambda (var) `(cons ',var ,var)) vars)))
               ,@vars))))

;;; ---------------------------------------------------------------------------

(defmethod process-cases-form ((type (eql :map)) &rest forms)
  (let ((vars (ensure-list (flatten (vars-from-assignment forms))))
        (values (values-from-assignment forms)))
    `(:b ,@(apply #'mapcar
                  (lambda (&rest args)
                    (mapcar (lambda (var value)
                              (cons var value))
                            vars args))
                  values))))

;;; ---------------------------------------------------------------------------

(defmethod process-cases-form ((type (eql :cross)) &rest forms)
  (let ((vars (ensure-list (flatten (vars-from-assignment forms))))
        (values (values-from-assignment forms))
        (result nil))
    (iterate-over-indexes
     (mapcar #'length values)
     (lambda (indexes)
       (let ((datum nil))
         (mapcar (lambda (name var index)
                   (push (cons name (elt var index)) datum))
                 vars
                 values
                 indexes)
         (push (nreverse datum) result))) 
     :right)
    `(:b ,@(nreverse result))))

;;; ---------------------------------------------------------------------------

(defun vars-from-assignment (assignment)
  (cond ((is-binding-p assignment)
         (mapcar #'car (second assignment)))
        ((metatilities:dotted-pair-p assignment)
         (car assignment))
        ((atom (car assignment))
         (car assignment))
        ((length-1-list-p assignment)
         (vars-from-assignment (first assignment)))
        (t (loop for assignment in assignment collect
                 (vars-from-assignment assignment)))))
         
;;; ---------------------------------------------------------------------------

(defun values-from-assignment (assignment)
  (cond ((is-binding-p assignment)
         (apply #'mapcar (lambda (&rest bindings)
                   (mapcar (lambda (binding)
                             (cdr binding))
                           bindings))
                 (rest assignment)))
        ((dotted-pair-p assignment)
         (cdr assignment))
        ((atom (car assignment))
         (list (eval (first (rest assignment)))))
        (t 
         (loop for assignment in assignment nconc
                 (ensure-list (values-from-assignment assignment))))))

;;; ---------------------------------------------------------------------------

(defun is-binding-p (assignment)
  (eq (first assignment) :b))


#|


(export '(map-prototypes-of
          prototypes-of
          prototype-of
          prototype-exists-p))

;;; ---------------------------------------------------------------------------
;;; API
;;; ---------------------------------------------------------------------------

(defgeneric map-prototypes-of (fn thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric prototypes-of (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric prototype-of (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric prototype-exists-p (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------
;;; implementation
;;; ---------------------------------------------------------------------------

(defmethod map-prototypes-of :around (fn thing)
  (declare (ignore fn))
  (when (prototype-exists-p thing)
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod map-prototypes-of (fn (thing standard-class))
  (map-subclass-prototypes fn thing))

;;; ---------------------------------------------------------------------------

(defmethod map-prototypes-of (fn (thing built-in-class))
  (map-subclass-prototypes fn thing))

;;; ---------------------------------------------------------------------------

(defun map-subclass-prototypes (fn thing)
  (mopu:map-subclasses thing
                       (lambda (subclass)
                         (when (prototype-exists-p subclass)
                           (funcall fn (prototype-of subclass)))))
  (values))
   
;;; ---------------------------------------------------------------------------

(defmethod prototypes-of (thing)
  (containers:collect-using 'map-prototypes-of nil thing))

;;; ---------------------------------------------------------------------------

(defmethod prototype-exists-p (thing)
  ;; the expensive way to see if a prototype exists is to try and make one
  ;; and see if it works...
  (handler-case 
    (let ((creator-method (compute-applicable-methods #'prototype-of (list thing))))
      (when creator-method
        (let ((x (prototype-of thing)))
          (declare (optimize (safety 3) (debug 3) (speed 0) (space 0)))
          x
          (values t))))
    (error (c) (inspect c) nil)))

;;; ---------------------------------------------------------------------------

(defmethod prototype-of ((thing standard-class))
  (allocate-instance thing))

;;; ---------------------------------------------------------------------------

(defmethod prototype-of ((thing (eql 'fixnum)))
  (variates:integer-random variates:*random-generator* -10 10))


|#


(defvar *test-maximum-time* 2
  "Maximum number of seconds a process test is allowed to run before we give up.")

;;; ---------------------------------------------------------------------------

(pushnew :timeout *deftest-clauses*)

;;; ---------------------------------------------------------------------------

(add-code-block
 :timeout 1 :class-def
 (lambda () (def :timeout)) 
 '((setf (def :timeout) (cleanup-parsed-parameter value)))
 (lambda ()
   (pushnew 'process-test-mixin (def :superclasses))
   (push (def :timeout) (def :default-initargs))
   (push :maximum-time (def :default-initargs))
   nil))

;;; ---------------------------------------------------------------------------

(defclass process-test-mixin ()
  ((maximum-time :initform *test-maximum-time* 
                 :accessor maximum-time
                 :initarg :maximum-time)))

;;; ---------------------------------------------------------------------------

(defclass test-timeout-failure (test-failure)
  ((test-problem-kind :initform "Timeout" :allocation :class)))

;;; ---------------------------------------------------------------------------

(define-condition test-timeout-condition (test-condition) 
                  ((maximum-time :initform *test-maximum-time* 
                                 :accessor maximum-time
                                 :initarg :maximum-time))
  (:report (lambda (c s)
             (format s "Test ran out of time (longer than ~S-second~:P)" 
                     (maximum-time c))
             (call-next-method))))

;;; ---------------------------------------------------------------------------

(defmethod do-testing :around ((test-suite process-test-mixin) result fn)
  (declare (ignore fn))
  (metatilities:with-timeout ((maximum-time test-suite)
                              (report-test-problem
                               'test-timeout-failure result test-suite (current-method test-suite)
                               (make-instance 'test-timeout-condition
                                 :maximum-time (maximum-time test-suite)))
                              result)
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod more-prototypes-p :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'more-prototypes-p))

;;; ---------------------------------------------------------------------------

(defmethod initialize-prototypes :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'initialize-prototypes))

;;; ---------------------------------------------------------------------------

(defmethod next-prototype :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'next-prototype))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-teardown :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'testsuite-teardown))

;;; ---------------------------------------------------------------------------

(defmethod start-test :before ((result test-result) (test-suite test-mixin) method-name)
  (declare (ignore method-name)) 
  (setf (current-step test-suite) 'start-test))

;;; ---------------------------------------------------------------------------

(defmethod end-test :before ((result test-result) (test-suite test-mixin) method-name)
  (declare (ignore method-name))
  (setf (current-step test-suite) 'end-test))

;;; ---------------------------------------------------------------------------

(defmethod setup-test :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'setup-test))

;;; ---------------------------------------------------------------------------

#+Ignore
(defmethod teardown-test :before ((test-suite test-mixin))
  (setf (current-step test-suite) 'teardown-test))


#|

(deftestsuite test-timeout ()
  ()
  (:timeout 1))

(addtest (test-timeout)
  test-should-pass
  (sleep 0.5)
  (ensure t))

(addtest (test-timeout)
  test-should-fail
  (sleep 1.5)
  (ensure t))
|#