(in-package #:lift)

(setf (documentation 'get-backtrace-as-string 'function)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace.")

(defun ensure-directory (pathname)
  (merge-pathnames #+clisp
		   (make-pathname :name "" :type "")
		   #-clisp
		   (make-pathname :name :unspecific :type :unspecific)
		   pathname))

(defun writable-directory-p (directory)
  (let ((directory (ensure-directory directory)))
    (and (probe-file directory)
	 #+allegro
	 (excl.osi:access directory excl.osi:*w-ok*))))

;; Handle missing platforms gracefully?
(defun total-bytes-allocated ()
  (if (fboundp '%total-bytes-allocated)
    (funcall '%total-bytes-allocated)
    0))

#+allegro
(defun %total-bytes-allocated ()
  (sys::gsgc-totalloc-bytes t))

#+(or digitool openmcl ccl)
(defun %total-bytes-allocated ()
  (ccl::total-bytes-allocated))

#+sbcl
(defun %total-bytes-allocated ()
  (cl-user::get-bytes-consed))

#+(or cmu scl)
(defun %total-bytes-allocated ()
  (ext:get-bytes-consed))

#+lispworks
;; thanks to Frank Schorr, via e-mail
(defun %total-bytes-allocated ()
  (hcl:total-allocation))

#+(or mcl ccl)
(defun get-backtrace-as-string (error)
  (with-output-to-string (s)
    (let ((*debug-io* s))
      (format *terminal-io* "~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
              error)
      (ccl:print-call-history :detailed-p nil))))

(defun get-backtrace (error)
  (get-backtrace-as-string error))

#+allegro
(defun get-backtrace-as-string (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (*print-miser-width* 40)
            (*print-pretty* t)
            (tpl:*zoom-print-circle* t)
            (tpl:*zoom-print-level* nil)
            (tpl:*zoom-print-length* nil))
	#+(or)
        (cl:ignore-errors
          (format *terminal-io* "~&~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
                  error))
        (cl:ignore-errors
          (let ((*terminal-io* s)
                (*standard-output* s))
            (tpl:do-command "zoom"
                            :from-read-eval-print-loop nil
                            :count t
                            :all t)))))))

#+(or)
(defun zoom-to-stream (condition output)
  (with-standard-io-syntax
    (let ((*print-readably* nil)
	  (*print-miser-width* 40)
	  (*print-pretty* t)
	  (tpl:*zoom-print-circle* t)
	  (tpl:*zoom-print-level* nil)
	  (tpl:*zoom-print-length* nil))
      (ignore-errors 
	(format *terminal-io* "Creating backtrace for ~a to ~a" 
		condition output))
      (flet ((zoom (s)
	       (ignore-errors
		 (let ((*terminal-io* s)
		       (*standard-output* s))
		   (tpl:do-command "zoom"
		     :from-read-eval-print-loop nil
		   :count t :all t)))))
	(cond ((streamp output)
	       (zoom output))
	      (t
	       (ensure-directories-exist output)
	       (with-open-file (s output :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
		 (zoom s))))))))

#+lispworks
(defun get-backtrace-as-string (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((dbg::*debugger-stack* (dbg::grab-stack nil :how-many most-positive-fixnum))
          (*debug-io* s)
          (dbg:*debug-print-level* nil)
          (dbg:*debug-print-length* nil))
      (dbg:bug-backtrace nil))))

#+sbcl
;; determine how we're going to access the backtrace in the next
;; function
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "*DEBUG-PRINT-VARIABLE-ALIST*" :sb-debug)
    (pushnew :hunchentoot-sbcl-debug-print-variable-alist *features*)))

#+sbcl
(defun get-backtrace-as-string (error)
  (declare (ignore error))
  (with-output-to-string (s)
    #+:hunchentoot-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-variable-alist*
            (list* '(*print-level* . nil)
                   '(*print-length* . nil)
                   sb-debug:*debug-print-variable-alist*)))
      (sb-debug:backtrace most-positive-fixnum s))
    #-:hunchentoot-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-level* nil)
          (sb-debug:*debug-print-length* nil))
      (sb-debug:backtrace most-positive-fixnum s))))

#+clisp
(defun get-backtrace-as-string (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (system::print-backtrace :out s)))

#+abcl
(defun get-backtrace (error)
  (declare (ignore error))
  (sys::backtrace-as-list))
	   

#+(or cmucl scl)
(defun get-backtrace-as-string (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((debug:*debug-print-level* nil)
          (debug:*debug-print-length* nil))
      (debug:backtrace most-positive-fixnum s))))





#+allegro
(defun cancel-current-profile (&key force?)
  (when (prof::current-profile-actual prof::*current-profile*)
    (unless force?
      (assert (member (prof:profiler-status) '(:inactive))))
    (prof:stop-profiler)
    (setf prof::*current-profile* (prof::make-current-profile))))

#+allegro
(defun current-profile-sample-count ()
   (ecase (prof::profiler-status :verbose nil)
    ((:inactive :analyzed) 0)
    ((:suspended :saved)
     (slot-value (prof::current-profile-actual prof::*current-profile*) 
		 'prof::samples))
    (:sampling (warn "Can't determine count while sampling"))))

#+allegro
(defun show-flat-profile (output)
  (let ((prof:*significance-threshold* 
	 (or *profiling-threshold* prof:*significance-threshold*)))
    (prof:show-flat-profile :stream output)))

#+allegro
(defun show-call-graph (output)
  (let ((prof:*significance-threshold* 
	 (or *profiling-threshold* prof:*significance-threshold*)))
    (prof:show-call-graph :stream output)))

#+allegro
(defun show-call-counts (output)
  (format output "~%~%Call counts~%")
  (let ((*standard-output* output))
    (prof:show-call-counts)))

#-allegro
(defun current-profile-sample-count ()
  0)

#-allegro
(defun show-flat-profile (output)
  (format output "~%~%Flat profile: unavailable for this Lisp~%"))

#-allegro
(defun show-call-graph (output)
  (format output "~%~%Call graph: unavailable for this Lisp~%"))

#-allegro
(defun show-call-counts (output)
  (format output "~%~%Call counts: unavailable for this Lisp~%"))

#-allegro
;; ugh!
(defun with-profile-report-fn 
    (name style fn body &key
     (log-name *log-path*)
     (count-calls-p *count-calls-p*)
     (timeout nil)
     destination)
  (declare (ignorable name style fn body log-name count-calls-p timeout destination))
  (funcall fn))
