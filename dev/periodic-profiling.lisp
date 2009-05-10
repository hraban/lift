(in-package #:lift)

(defvar *periodic-profilers* (make-hash-table))

(defstruct (periodic-profiler (:conc-name periodic-profiler-)
			      (:print-object print-periodic-profiler))
  function
  period
  (profile-style :time)
  (report-name nil)
  (last-active 0)
  (last-profile-style nil))

(defun print-periodic-profiler (object stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a every ~a" 
	    (periodic-profiler-function object)
	    (periodic-profiler-period object))))

(excl:def-fwrapper profile-this-periodically (&rest args)
  (declare (ignorable args)
	   (dynamic-extent args))
  (let* ((spot (%find-periodic-profile-spot excl::primary-function))
	 (pp (first spot)))
    (if (> (- (get-universal-time) (periodic-profiler-last-active pp))
	   (periodic-profiler-period pp))
	(let* ((style (periodic-profiler-profile-style pp)))
	  (setf (periodic-profiler-last-profile-style pp)
		(if (eq style :alternating)
		    (case (periodic-profiler-last-profile-style pp)
		      (:time :space)
		      ((:space nil) :time))
		    style))
	  (with-profile-report 
	      ((periodic-profiler-report-name pp)
	       (periodic-profiler-last-profile-style pp))
	    (excl:call-next-fwrapper))
	  (setf (periodic-profiler-last-active pp) (get-universal-time)))
	(excl:call-next-fwrapper))))

(defun start-periodic-profiling (report-name function &key (period 10) 
				 (profile-style :time))
  (assert (find profile-style '(:time :space :alternating)))
  (let ((spot (%find-periodic-profile-spot function)))
    (setf (first spot) (make-periodic-profiler 
			:report-name report-name
			:function function 
			:period period
			:profile-style profile-style)
	  (second spot) t)
    (excl:fwrap function 
		:periodic-profiler 'profile-this-periodically)))

(defun stop-periodic-profiling (function)
  (setf (second (%find-periodic-profile-spot function)) nil)
  (excl:funwrap function :periodic-profiler))

(defun %find-periodic-profile-spot (function)
  (setf function (coerce function 'function))
  (multiple-value-bind (spot found?)
      (gethash function *periodic-profilers*)
    (if found? 
	spot
	(setf (gethash function *periodic-profilers*)
	      (list 0 nil)))))

(defun periodic-profilers ()
  (let ((result nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (when (and (consp v) (second v))
		 (let ((pp (first v)))
		   (push (list (periodic-profiler-function pp)
			       (periodic-profiler-period pp)
			       (periodic-profiler-profile-style pp))
			 result))))
	     *periodic-profilers*)
    result))

#|

(periodic-profilers)

(defun test-pp ()
  (let ((x nil))
    (while-counting-events (0.5)
      (loop for i from 0 do (setf x i)))
    x))

(test-pp)

(with-profile-report ('test-pp :time) (test-pp))


(start-periodic-profiling
 'test-pp 'test-pp :period 1.5 :profile-style :alternating)

(stop-periodic-profiling
 'test-pp)

(loop repeat 20
     for i from 0 do
     (print i)
     (test-pp))

|#
