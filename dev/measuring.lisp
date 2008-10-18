(in-package #:lift)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmeasure measure-seconds 
    :value 'get-internal-real-time
    :finally '(coerce (/ it internal-time-units-per-second) 
			'double-float)
    :type integer)

(defmeasure measure-space
    :value 'total-bytes-allocated
    :type integer)

)

#+(or)
(while-measuring-1 (conses measure-space)
  (while-measuring-1 (time measure-seconds)
    blay))

#+(or)
(let ((time 0))
  (while-measuring-1 (time measure-seconds)
    (sleep 1))
  time)

#+(or)
(let ((conses 0))
  (while-measuring-1 (conses measure-space)
    (sleep 1))
  conses)

#+(or)
(while-measuring (measure-seconds)
  (sleep 1))

(defmacro with-measuring ((var measure-fn) &body body)
  (let ((ginitial (gensym "value-"))
	(gcondition (gensym "condition-")))
    `(let ((,ginitial (,measure-fn))
	   (,gcondition nil))
       (prog1
	   (handler-case 
	       (progn ,@body)
	     (error (c) (setf ,gcondition c)))
	 (setf ,var (- (,measure-fn) ,ginitial))
	 (when ,gcondition (error ,gcondition))))))

#+(or)
;; remove
(defmacro measure-time ((var) &body body)
  `(prog1
       (with-measuring (,var get-internal-real-time)
	 ,@body)
     (setf ,var (coerce (/ ,var internal-time-units-per-second) 
			'double-float))))

(defmacro measure-time ((var) &body body)
  `(while-measuring-1 (,var measure-seconds) ,@body))

#+(or)
(let ((time 0))
  (while-measuring-1 (time measure-seconds)
    (sleep 1))
  time)

#+(or)
;; remove
(defmacro measure-conses ((var) &body body)
  `(with-measuring (,var total-bytes-allocated)
     ,@body))

(defmacro measure-conses ((var) &body body)
  `(while-measuring-1 (,var measure-space) ,@body))

(defun measure-fn (fn &rest args)
  (declare (dynamic-extent args))
  (let ((bytes 0) (seconds 0) result)
    (measure-time (seconds)
      (measure-conses (bytes)
	(setf result (apply fn args))))
    (values seconds bytes result)))

(defmacro measure (seconds bytes &body body)
  (let ((result (gensym)))
    `(let (,result)
       (measure-time (,seconds)
	 (measure-conses (,bytes)
	   (setf ,result (progn ,@body))))
       (values ,result))))

(defmacro measure-time-and-conses (&body body)
  (let ((seconds (gensym))
	(conses (gensym))
	(results (gensym)))
    `(let ((,seconds 0) (,conses 0) ,results) 
       (setf ,results (multiple-value-list 
			    (measure ,seconds ,conses ,@body)))
       (values-list (nconc (list ,seconds ,conses)
			   ,results)))))

(defvar *functions-to-profile* nil)

(defvar *additional-markers* nil)

(defvar *profiling-threshold* nil)

(defun make-profiled-function (fn)
  (lambda (style count-calls-p)
    (declare (ignorable style count-calls-p))
    #+allegro
    (prof:with-profiling (:type style :count count-calls-p)
      (funcall fn))
    #-allegro
    (funcall fn)))

(defun generate-profile-log-entry (log-name name seconds conses results error)
  (ensure-directories-exist log-name)
  ;;log 
  (with-open-file (output log-name
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :append)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
	(terpri output)
	(format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~} ~s ~s ~a\)"
		(date-stamp :include-time? t) name 
		seconds conses *additional-markers*
		results (current-profile-sample-count)
		error)))))

#+allegro
(defun with-profile-report-fn 
    (name style fn &key (log-name *benchmark-log-path*)
			       (count-calls-p *count-calls-p*)
			       (timeout nil))
  (assert (member style '(:time :space :count-only)))
  (cancel-current-profile :force? t)
  (let* ((seconds 0.0) (conses 0)
	 results)
    (unwind-protect
	 (handler-case
	     (with-timeout (timeout)
	       (setf results
		     (multiple-value-list
		      (prof:with-profiling (:type style :count count-calls-p)
			(measure seconds conses (funcall fn))))))
	   (timeout-error 
	       (c)
	     (declare (ignore c))))
      ;; cleanup / ensure we get report
      (ensure-directories-exist log-name)
      ;;log 
      (with-open-file (output log-name
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	(with-standard-io-syntax
	  (let ((*print-readably* nil))
	    (terpri output)
	    (format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~} ~s ~s\)"
		    (date-stamp :include-time? t) name 
		    seconds conses *additional-markers*
		    results (current-profile-sample-count)))))
      (when (> (current-profile-sample-count) 0)
	(let ((pathname (unique-filename
			 (merge-pathnames
			  (make-pathname 
			   :type "prof"
			   :name (format nil "~a-~a-" name style))
			  log-name))))
	  (let ((prof:*significance-threshold* 
		 (or *profiling-threshold* 0.01)))
	    (format t "~&Profiling output being sent to ~a" pathname)
	    (with-open-file (output pathname
				    :direction :output
				    :if-does-not-exist :create
				    :if-exists :append)
	      (format output "~&Profile data for ~a" name)
	      (format output "~&Date: ~a" 
		      (excl:locale-print-time (get-universal-time)
					      :fmt "%B %d, %Y %T" :stream nil))
	      (format output "~&  Total time: ~,2F; Total space: ~:d \(~:*~d\)"
		      seconds conses)
	      (format output "~%~%")
	      (when (or (eq :time style)
			(eq :space style))
		(prof:show-flat-profile :stream output)
		(prof:show-call-graph :stream output)
		(when count-calls-p
		  (format output "~%~%Call counts~%")
		  (let ((*standard-output* output))
		    (prof:show-call-counts))))
	      (when *profile-extra*
		(loop for thing in *profile-extra* do
		     (format output "~%~%")
		     (let ((*standard-output* output))
		       (prof:disassemble-profile thing)))))))))
    (values-list results)))

(defmacro while-counting-repetitions ((&optional (delay 1.0)) &body body)
  "Execute `body` repeatedly for `delay` seconds. Returns the number
of times `body` is executed per second. Warning: assumes that `body` will not
be executed more than a fixnum number of times. The `delay` defaults to
1.0."
  (let ((gevent-count (gensym "count-"))
	(gdelay (gensym "delay-"))
	(gignore (gensym "ignore-"))
	(gfn (gensym "fn-")))
    `(let ((,gfn
	    (compile
	     nil
	     (lambda () 
	       (let ((,gevent-count 0)
		     (,gdelay ,delay))
		 (declare (type fixnum ,gevent-count))
		 (handler-case
		     (lift::with-timeout (,gdelay)
		       (loop
			  (progn ,@body)
			  (setf ,gevent-count (the fixnum (1+ ,gevent-count)))))
		   (lift::timeout-error (,gignore)
		     (declare (ignore ,gignore))
		     (if (plusp ,gevent-count)
			 (float (/ ,gevent-count ,gdelay))
			 ,gevent-count))))))))
	   (funcall ,gfn))))
  
(defun count-repetitions (fn delay)
  "Funcalls `fn` repeatedly for `delay` seconds. Returns the number
of times `fn` was called. Warning: the code assumes that `fn` will 
not be called more than a fixnum number of times."
  (let ((event-count 0)
	(compiled-fn (compile nil fn)))
    (declare (type fixnum event-count))
    (handler-case
	(lift::with-timeout (delay) 
	  (loop 
	     (funcall compiled-fn)
	     (setf event-count (the fixnum (1+ event-count)))))
      (lift::timeout-error (c)
	(declare (ignore c))
	(if (plusp event-count)
	    (/ event-count delay)
	    event-count)))))

#+test
(defun fibo (n)
  (cond ((< n 2)
	 1)
	(t
	 (+ (fibo (- n 1)) (fibo (- n 2))))))

#+test
(with-profile-report ('test :time) 
  (loop for i from 1 to 10 do
       (fibo i))
  (loop for i from 10 downto 1 do
       (fibo i)))

