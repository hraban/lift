(in-package #:lift)

;; dribble
;; full output for all tests on separate pages per suite? whatever.
;; test environment

#|
For *standard-input*: an input stream

For *error-output*, *standard-output*, and *trace-output*: an output stream.

For *debug-io*, *query-io*: a bidirectional stream.
|#

#|

  (start-report-output result stream format)
  (summarize-test-result result stream format)
  (summarize-test-environment result stream format)
  (when (or (failures result) (errors result)
	    (expected-failures result) (expected-errors result)
	    (skipped-test-cases result))
    (summarize-test-problems result stream format))
  (summarize-tests-run result stream format)
  (end-report-output result stream format)
  (generate-detailed-reports result stream format))

lift::(progn
  (setf (test-result-property *test-result* :style-sheet) "test-style.css")
  (setf (test-result-property *test-result* :title) "lubm-50")
  (setf (test-result-property *test-result* :unique-name) t)
  (test-result-report *test-result* #p "/fi/internal/people/gwking/agraph/testing/report/2008-08-21-lubm-50-prolog" :html))

lift::(progn
	(setf (test-result-property *test-result* :style-sheet) 
	      "test-style.css")
	(setf (test-result-property *test-result* :title)
	      "Ugh")
	(setf (test-result-property *test-result* :if-exists)
	      :error)
	(test-result-report *test-result*  #p"report-20080813a.sav" :save))
	
(run-tests :suite '(lift-test test-cursors))

(run-tests :suite 'lift-test-ensure)

(test-result-property *test-result* :title)
|#


(defvar *log-header-hooks* nil)
(defvar *log-footer-hooks* nil)
(defvar *log-detail-hooks* nil)

(defvar *report-hooks* nil)

(defun report-hooks-for (mode)
  (cdr (assoc mode *report-hooks*)))

(defun (setf report-hooks-for) (value mode)
  (setf *report-hooks* (remove mode *report-hooks* :key 'car))
  (push (cons mode value) *report-hooks*)
  value)

(defun add-report-hook-for (mode hook)
  (setf (report-hooks-for mode) (push hook (report-hooks-for mode))))

(defgeneric start-report-output (result stream format)
  )

(defgeneric summarize-test-result (result stream format)
  )

(defgeneric summarize-test-environment (result stream format)
  )

(defgeneric summarize-test-problems (result stream format)
  )

(defgeneric summarize-test-problems-of-type 
    (format problems stream id heading name)
  )

(defgeneric write-log-test 
    (format suite-name test-case-name data &key stream)
  )

(defgeneric generate-detailed-reports (result stream format)
  )

(defgeneric summarize-tests-run (result stream format)
 )

(defgeneric end-report-output (result stream format)
 )

(defgeneric html-header (stream title style-sheet)
 )

;; when it doubt, add a special
(defvar *report-environment* nil
  "Used internally by LIFT reports.")

(defun make-report-environment ()
  nil)

;; env variables need to be part saved in result

(defgeneric test-result-report (result output format
			       &key package &allow-other-keys)
  )

(defmethod test-result-report (result output format
			       &rest args
			       &key (package *package*) &allow-other-keys)
  (declare (ignore args))
  (let ((*report-environment* (make-report-environment))
	(*package* (or (find-package package) *package*)))
    (cond ((or (stringp output)
	       (pathnamep output))
	   (with-open-file (stream 
			    output
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists (or (test-result-property
					    result :if-exists)
					   :error))
	     (%test-result-report-stream result stream format)))
	  ((streamp output)
	   (%test-result-report-stream result output format))
	  ((eq output t)
	   (%test-result-report-stream result *standard-output* format))
	  (t
	   (error "Don't know how to send a report to ~s" output)))))

(defun %test-result-report-stream (result stream format)
  (start-report-output result stream format)
  (summarize-test-result result stream format)
  (summarize-test-environment result stream format)
  (when (or (failures result) (errors result)
	    (expected-failures result) (expected-errors result)
	    (skipped-test-cases result))
    (summarize-test-problems result stream format))
  (summarize-tests-run result stream format)
  (end-report-output result stream format)
  (generate-detailed-reports result stream format))

(defmethod start-report-output (result stream format)
  (declare (ignore result stream format))
  )

(defmethod summarize-test-result (result stream format)
  (declare (ignore format))
  (format stream"~&Test results for: ~a~%"
	  (results-for result))
  (let ((complete-success? (and (null (errors result))
                                (null (failures result))))) 
    (cond (complete-success?
	   (format stream"~&~A Successful test~:P~%"
		   (length (tests-run result))))
	  (t
	   (format stream "~&~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~].~%"
			(length (tests-run result))
			(length (failures result))
			(length (errors result)))))))

(defmethod summarize-test-environment (result stream format)
  (format stream "~&Lisp: ~a (~a)" 
	  (lisp-version-string) (lisp-implementation-version))
  (format stream "~&On  : ~a ~a ~a" 
	  (machine-type) (machine-version) (machine-instance))
  (terpri stream)
  (let ((*standard-output* stream))
    (loop for hook in (report-hooks-for :summarize-environment) do
	 (funcall hook result format)))
  (terpri stream)
  )

(defmethod summarize-test-problems (result stream format)
  (declare (ignore result stream format))
  )

(defmethod generate-detailed-reports (result stream format)
  (declare (ignore result stream format))
  )

(defmethod summarize-tests-run (result stream format)
  (declare (ignore result stream format)))

(defmethod end-report-output (result stream format)
  (declare (ignore result stream format))
  )

#+(or)
(defun summarize-test-environment (result stream format)
  (loop for symbol in (sort `((*lift-dribble-pathname*)
			      (*lift-debug-output* interpret-lift-stream)
			      (*lift-standard-output* interpret-lift-stream)
			      (*test-break-on-errors?*)
			      (*test-do-children?*)
			      (*lift-equality-test*)
			      (*test-print-length*)
			      (*test-print-level*)
			      (*lift-if-dribble-exists*))
			    'string-lessp :key 'first) do

       (print)))


;; some cruft stolen from cl-markdown
(defvar *html-meta*
  '((name (:author :description :copyright :keywords :date))
    (http-equiv (:refresh :expires))))

(defmethod start-report-output (result stream (format (eql :html)))
  (html-header 
   stream 
   (test-result-property result :title)
   (test-result-property result :style-sheet)))

(defmethod html-header (stream title style-sheet)
  (format stream "~&<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
  (format stream "~&<html>~&<head>")
  (when title
    (format stream "~&<title>~a</title>" title))
  (when style-sheet 
    (unless (search ".css" style-sheet)
      (setf style-sheet (concatenate 'string style-sheet ".css")))
    (format stream "~&<link type='text/css' href='~a' rel='stylesheet' />"
	    style-sheet))
  (format stream "~&</head>~&<body>"))

(defmethod summarize-test-result (result stream (format (eql :html)))
  (format stream "~&<div id=\"summary\">")
  (format stream "~&<h1>Test results for: ")
  (cond ((ignore-errors (probe-file (results-for result)))
	 (let ((config-html (merge-pathnames "config.html" stream)))
	   (save-configuration-file result config-html)
	   (format stream "<a href=\"config.html\">~a</a>"
		   (results-for result))))
	(t
	 (format stream "~a" (results-for result))))
  (format stream "</h1>~%")
  (let ((complete-success? (and (null (errors result))
                                (null (failures result))))) 
    (cond (complete-success?
	   (format stream "~&<h2>~A Successful test~:P</h2>~%"
		   (length (tests-run result))))
	  (t
	   (format stream "~&<h2>~:d test~:p"
		   (length (tests-run result)))
	   (format stream "~[~:;, ~:*<a href=\"#failures\">~:d failure~:P~]</a>"
		   (length (failures result)))
	   (format stream "~[~:;, ~:*<a href=\"#errors\">~:d error~:P~]</a>"
		   (length (errors result)))
	   (format stream "</h2>")))

    (when (or (expected-errors result) (expected-failures result))
      (format stream "~&<h3>")
      (format stream "~[~:;~:*<a href=\"#expected-failures\">Expected failure~p: ~:*~:d</a>~]"
	      (length (expected-failures result)))
      ;; zero if only one or the other (so we don't need a separator...)
      (format stream "~[~:;, ~]"
	      (* (length (expected-failures result))
		 (length (expected-errors result))))
      (format stream "~[~:;~:*<a href=\"#expected-errors\">Expected error~p: ~:*~:d</a>~]"
	      (length (expected-errors result)))
      (format stream "</h3>~%"))

    (when (or (skipped-test-cases result) (skipped-testsuites result))
      (format stream "~&<h3>")
      (when (skipped-test-cases result)
	(format stream "~:d Skipped test cases" 
		(length (skipped-test-cases result)))
	(when (skipped-testsuites result)
	  (format stream ", ")))
      (when (skipped-testsuites result)
	(format stream "~:d Skipped test suites" 
		(length (skipped-testsuites result))))
      (format stream "</h3>~%"))

    (when (and (slot-boundp result 'end-time-universal)
	       (numberp (end-time-universal result))
	       (numberp (start-time-universal result)))
      (format stream "~&<h3>Testing took: ~:d seconds</h3>"
	      (- (end-time-universal result)
		 (start-time-universal result))))
    #+(or)
    (when (and (numberp (real-end-time result))
	       (numberp (real-start-time result)))
      (format stream "~&Time: ~,2f real-time"
	      (/ (- (real-end-time result) (real-start-time result))
		 internal-time-units-per-second))))
  (format stream "~&</div>"))

(defmethod summarize-test-environment (result stream (format (eql :html)))
  (declare (ignore result))
  (format stream "~&<div id=\"environment\">")
  (call-next-method)
  (format stream "~&</div>"))

(defmethod summarize-test-problems (result stream  (format (eql :html)))
  (format stream "~&<div id=\"problem-summary\">")
  (format stream "~&<h2>Problem Summary:</h2>")
  (when (failures result)
    (summarize-test-problems-of-type 
     format (failures result) stream "failure-summary" "Failures" "failures"))
  (when (errors result)
    (summarize-test-problems-of-type 
     format (errors result) stream "error-summary" "Errors" "errors"))
  (when (expected-failures result)
    (summarize-test-problems-of-type 
     format (expected-failures result)
     stream "expected-failure-summary" "Expected Failures" "expected-failures"))
  (when (expected-errors result)
    (summarize-test-problems-of-type 
     format (expected-errors result) stream "expected-failure-summary" 
     "Expected Errors" "expected-errors"))
  (when (skipped-test-cases result)
    (summarize-test-problems-of-type 
     format (skipped-test-cases result) stream "skipped-cases-summary" 
     "Skipped tests" "skipped-tests"))
  (format stream "~&</div>"))

(defmethod summarize-test-problems-of-type 
    (format problems stream id heading name)
  (format stream "~&<div id=\"~a\">" id)
  (format stream "~&<a name=\"~a\"></a><h3>~a</h3>" name heading)
  (report-tests-by-suite 
   format  
   (mapcar (lambda (problem)
	     `(,(testsuite problem)
		,(test-method problem)
		(:problem ,problem)))
	   problems)
   stream)
  (format stream "~&</div>"))

(defmethod summarize-tests-run (result stream (format (eql :html)))
  (flet ((doit (stream)
	   (format stream "~&<div id=\"results\">")
	   (format stream "~&<h2>Tests Run:</h2>")
	   (report-tests-by-suite format (tests-run result) stream)
	   (format stream "~&</div>")))
    (cond ((or (failures result) (errors result)
	       (expected-failures result) (expected-errors result))
	   ;; print separately
	   (with-open-file (new-stream (merge-pathnames "summary.html" stream)
				   :direction :output
				   :if-does-not-exist :create
				   :if-exists :supersede)
	     (html-header 
	      new-stream 
	      "test summary"
	      (test-result-property result :style-sheet))
	     (format new-stream "~&<br><br><h1>Test Summary</h1>~%")
	     (format new-stream "~&<a href=\"~a\">Back</a>"
		     (namestring (make-pathname :name (pathname-name stream)
						:type (pathname-type stream))))
	     (doit new-stream)
	     (html-footer new-stream))
	   (format stream
		   "~&<h2><a href=\"summary.html\">Test result summary</a></h2>~%")
	   (when (errors result)
	     (build-issues-report result :errors stream))
	   (when (failures result)
	     (build-issues-report result :failures stream))
	   (when (expected-failures result)
	     (build-issues-report result :expected-failures stream))
	   (when (expected-errors result)
	     (build-issues-report result :expected-errors stream))
	   (when (skipped-test-cases result)
	     (build-issues-report result :skipped-testsuites stream)))	
	  (t
	   (doit stream)))))


(defmethod report-test-suite-by-suite 
    (format stream remaining current-suite suite)
  (declare (ignore format stream remaining current-suite suite))
  )

(defmethod report-test-case-by-suite (format stream suite test-name datum)
  (declare (ignore format stream suite test-name datum))
  )

(defmethod finish-report-tests-by-suite (format stream current-suite)
  (declare (ignore format stream current-suite))
  )

(defmethod report-test-suite-by-suite 
    ((format (eql :html)) stream remaining current-suite suite)
  (when current-suite
    (format stream "</div>"))
  (setf current-suite suite)
  (format stream "~&<div class=\"testsuite\">")
  (let* ((this-suite-end (or 
			  (position-if 
			   (lambda (datum)
			     (not (eq current-suite (first datum))))
			   remaining)
			  (length remaining)))
	 (error-count (count-if 
		       (lambda (datum)
			 (and (getf (third datum) :problem)
			      (typep (getf (third datum) :problem)
				     'test-error)))
		       remaining
		       :end this-suite-end))
	 (failure-count (count-if 
			 (lambda (datum)
			   (and (getf (third datum) :problem)
				(typep (getf (third datum) :problem)
				       'test-failure)))
			 remaining
			 :end this-suite-end))
	 (extra-class (cond ((and (= error-count 0) (= failure-count 0))
			     'testsuite-all-passed)
			    ((> error-count 0)
			     'testsuite-some-errors)
			    (t
			     'testsuite-some-failures))))
    (format stream "~&<div class=\"testsuite-title\"><table class=\"~a\"><tr><td>~a</td>" extra-class suite)
    (format stream "<td class=\"testsuite-test-count\">~:d test~:p</td>"
	    (test-case-count current-suite))
    (format stream "<td class=\"testsuite-summary\">")
    (cond ((and (= error-count 0) (= failure-count 0))
	   (format stream "all passed"))
	  (t
	   (format stream "~[~:;~:*~:d failure~:p~]" 
		   failure-count)
	   (when (and (> error-count 0) (> failure-count 0))
	     (format stream ", "))
	   (format stream  "~[~:;~:*~a error~:p~]" 
		   error-count)))
    (format stream "</td></tr></table>")
    (format stream "</div>")))

(defmethod report-test-case-by-suite 
    ((format (eql :html)) stream suite test-name datum)
  (format stream "~&<div class=\"test-case\">")
  (let ((problem (getf datum :problem)))
    (cond ((typep problem 'test-failure)
	   (format stream "~&<span class=\"test-name\"><a href=\"~a\" title=\"details\">~a</a></span>"
		   (details-link stream suite test-name)
		   test-name)
	   (format stream 
		   "~&<span class=\"test-failure\">failure</span>" ))
	  ((typep problem 'test-error)
	   (format stream "~&<span class=\"test-name\"><a href=\"~a\" title=\"details\">~a [during ~a]</a></span>"
		   (details-link stream suite test-name)
		   test-name
		   (test-step problem))
	   (format stream "~&<span class=\"test-error\">error</span>"))
	  (t
	   (format stream "~&<span class=\"test-name\">~a</span>" 
		   test-name)
	   (let ((seconds (getf datum :seconds))
		 (conses (getf datum :conses)))
	     (when seconds 
	       (format stream "<span class=\"test-time\">~,3f</span>"
		       seconds))
	     (when conses 
	       (format stream "<span class=\"test-space\">~:d</span>"
		       conses)))))
    (format stream "~&</div>")))

(defmethod finish-report-tests-by-suite
    ((format (eql :html)) stream current-suite)
  (when current-suite
    (format stream "</div>")))

(defun report-tests-by-suite (format tests stream)
  (let ((current-suite nil))
    (loop for rest = (sort (copy-list tests)
			   'string-lessp :key 'first) then (rest rest) 
       while rest
       for (suite test-name datum) = (first rest) do
	 (unless (eq current-suite suite)
	   (report-test-suite-by-suite format stream rest current-suite suite)
	   (setf current-suite suite))
	 (report-test-case-by-suite format stream suite test-name datum))
    (finish-report-tests-by-suite format stream current-suite)))

#+(or)
(defun report-tests-by-suite (tests stream)
  (let ((current-suite nil))
    (loop for rest = (sort 
		      (copy-list tests)
		      'string-lessp :key 'first) then (rest rest) 
       while rest
       for (suite test-name datum) = (first rest) do
       (unless (eq current-suite suite)
	 (when current-suite
	   (format stream "</div>"))
	 (setf current-suite suite)
	 (format stream "~&<div class=\"testsuite\">")
	 (let* ((this-suite-end (or 
				 (position-if 
				  (lambda (datum)
				    (not (eq current-suite (first datum))))
				  rest)
				 (length rest)))
		(error-count (count-if 
			      (lambda (datum)
				(and (getf (third datum) :problem)
				     (typep (getf (third datum) :problem)
					    'test-error)))
			      rest
			      :end this-suite-end))
		(failure-count (count-if 
				(lambda (datum)
				  (and (getf (third datum) :problem)
				       (typep (getf (third datum) :problem)
					      'test-failure)))
				rest
				:end this-suite-end))
		(extra-class (cond ((and (= error-count 0) (= failure-count 0))
				    'testsuite-all-passed)
				   ((> error-count 0)
				    'testsuite-some-errors)
				   (t
				    'testsuite-some-failures))))
	   (format stream "~&<div class=\"testsuite-title\"><table class=\"~a\"><tr><td>~a</td>" extra-class suite)
	   (format stream "<td class=\"testsuite-test-count\">~:d test~:p</td>"
		   (test-case-count current-suite))
	   (format stream "<td class=\"testsuite-summary\">")
	   (cond ((and (= error-count 0) (= failure-count 0))
		  (format stream "all passed"))
		 (t
		  (format stream "~[~:;~:*~:d failure~:p~]" 
			  failure-count)
		  (when (and (> error-count 0) (> failure-count 0))
		    (format stream ", "))
		  (format stream  "~[~:;~:*~a error~:p~]" 
			  error-count)))
	   (format stream "</td></tr></table>")
	   (format stream "</div>")))
	 (format stream "~&<div class=\"test-case\">")
	 (let ((problem (getf datum :problem)))
	   (cond ((typep problem 'test-failure)
		  (format stream "~&<span class=\"test-name\"><a href=\"~a\" title=\"details\">~a</a></span>"
			  (details-link stream suite test-name)
			  test-name)
		  (format stream 
			  "~&<span class=\"test-failure\">failure</span>" ))
		 ((typep problem 'test-error)
		  (format stream "~&<span class=\"test-name\"><a href=\"~a\" title=\"details\">~a [during ~a]</a></span>"
			  (details-link stream suite test-name)
			  test-name
			  (test-step problem))
		  (format stream "~&<span class=\"test-error\">error</span>"))
		 (t
		  (format stream "~&<span class=\"test-name\">~a</span>" 
			  test-name)
		  (let ((seconds (getf datum :seconds))
			(conses (getf datum :conses)))
		    (when seconds 
		      (format stream "<span class=\"test-time\">~,3f</span>"
			      seconds))
		    (when conses 
		      (format stream "<span class=\"test-space\">~:d</span>"
			      conses)))))
	   (format stream "~&</div>")))
    (when current-suite
      (format stream "</div>"))))

(defun get-details-links-table ()
  (let ((hash (getf *report-environment* :details-links)))
    (or hash
	(setf (getf *report-environment* :details-links)
	      (make-hash-table :test 'equal)))))

#+(or)
(get-details-links-table)

(defun details-link (stream suite name)
  (declare (ignore stream))
  (let* ((hash (get-details-links-table)))
    (or (gethash (cons suite name) hash)
	(progn
	  (incf (getf *report-environment* :details-links-count 0))
	  (setf (gethash (cons suite name) hash)
		(make-pathname 
		 :name (format nil "details-~a" 
			       (getf *report-environment* :details-links-count))
		 :type "html"))))))

(defmethod end-report-output (result stream (format (eql :html)))
  (let ((style-sheet (test-result-property result :style-sheet)))
    (when style-sheet
      (ignore-errors
	(copy-file (asdf:system-relative-pathname 
		    'lift "resources/test-style.css")
		   (make-pathname 
		    :name (pathname-name style-sheet)
		    :type (pathname-type style-sheet)
		    :defaults (pathname stream))
		   :if-exists :supersede))))
  (html-footer stream))

(defun html-footer (stream)
  (format stream "<div id=\"footer\">")
  (format stream "~&generated on ~a" 
	  #+allegro
	  (excl:locale-print-time 
	   (get-universal-time)
	   :fmt "%B %d, %Y %T GMT%z" :stream nil)
	  #-allegro
	  (get-universal-time))
  (format stream "</div>")
  (format stream "~&</body></html>"))

(defmethod generate-detailed-reports (result stream (format (eql :html)))
  (loop for (suite test-name datum)  in (tests-run result)
     when (getf datum :problem) do
     (let ((output-pathname (merge-pathnames
			     (details-link stream suite test-name) 
			     stream)))
       (ensure-directories-exist output-pathname)
       (let ((*print-right-margin* 64)
	     (problem (getf datum :problem)))
	 (with-open-file (out output-pathname
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :supersede)
	   (html-header 
	    out 
	    (format nil "Test ~a details | ~a" 
		    test-name (test-result-property result :title))
	    (test-result-property result :style-sheet))
	   (format out "~&<h2>Test ~a details</h2>" test-name)
	   (format out "~&<a href=\"~a\">Back</a>"
		   (namestring (make-pathname :name (pathname-name stream)
					      :type (pathname-type stream))))
	   (format out "~&<p>Problem occurred during ~a.</p>"
		   (test-step problem))
	   (format out "~&<pre>")
	   (format out "~a"
		   (wrap-encode-pre 
		    (with-output-to-string (s)
		      (print-test-problem "" problem s t))
		    :width (test-result-property 
			    *test-result* :print-width 60)))
	   (format out "~&</pre>") 
	   (when (and (typep problem 'test-error-mixin)
		      (backtrace problem))
	     (format out "~&~%<h2>Backtrace</h2>~%~%")
	     (format out "~&<pre><code>~%")
	     (format out "~a"
		     (wrap-encode-pre 
		      (with-output-to-string (s)
			(print (backtrace problem) s))
		      :width (test-result-property 
			      *test-result* :print-width 60)))
	     (format out "~&</pre></code>~%"))
	   (html-footer out))))))

#+(or)
(defmethod summarize-test-environment (result stream format)
  (loop for symbol in (sort `((*lift-dribble-pathname*)
			      (*lift-debug-output* interpret-lift-stream)
			      (*lift-standard-output* interpret-lift-stream)
			      (*test-break-on-errors?*)
			      (*test-do-children?*)
			      (*lift-equality-test*)
			      (*test-print-length*)
			      (*test-print-level*)
			      (*lift-if-dribble-exists*))
			    'string-lessp :key 'first) do

       (print)))

(defun wrap-encode-pre (string &key (width 80))
  ;; Copied from CL-Markdown
  ;; Copied from HTML-Encode
  ;;?? this is very consy
  ;;?? crappy name
  (declare (simple-string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
	(column 0))
    (with-output-to-string (out output)
      (loop for char across string
	 do (case char
	      ((#\&) (incf column) (write-string "&amp;" out))
	      ((#\<) (incf column) (write-string "&lt;" out))
	      ((#\>) (incf column) (write-string "&gt;" out))
	      ((#\Tab #\Space #\Return #\Newline)
	       (cond ((or (>= column width) 
			  (char= char #\Return)
			  (char= char #\Newline))
		      (setf column 0)
		      (terpri out))
		     ((char= char #\Space)
		      (incf column)
		      (write-char char out))
		     ((char= char #\Tab)
		      (incf column 4)
		      (write-string "    " out))))
	      (t (incf column) (write-char char out)))))
    (coerce output 'simple-string)))

;;;;;

(defmethod summarize-test-result (result stream (format (eql :describe)))
  (describe result stream))

(defmethod summarize-tests-run (result stream (format (eql :detail)))
  (format stream "~&## Tests Run:")
  (let ((tests (tests-run result))
	(current-suite nil))
    (loop for rest = tests then (rest rest)
       while rest
       for (suite test-name datum) = (first rest) do
       (unless (eq current-suite suite)
	 (when current-suite
	   (format stream "~%~%"))
	 (setf current-suite suite)
	 (let* ((this-suite-end (or 
				 (position-if 
				  (lambda (datum)
				    (not (eq current-suite (first datum))))
				  rest)
				 (length rest)))
		(error-count (count-if 
			      (lambda (datum)
				(and (getf (third datum) :problem)
				     (typep (getf (third datum) :problem)
					    'test-error)))
			      rest
			      :end this-suite-end))
		(failure-count (count-if 
				(lambda (datum)
				  (and (getf (third datum) :problem)
				       (typep (getf (third datum) :problem)
					      'test-failure)))
				rest
				:end this-suite-end))
		#+(or)
		(extra-class (cond ((and (= error-count 0) (= failure-count 0))
				    'testsuite-all-passed)
				   ((> error-count 0)
				    'testsuite-some-errors)
				   (t
				    'testsuite-some-failures))))
	   (format stream "~%### ~a, ~d tests ~&" 
		   suite (test-case-count current-suite))
	   (cond ((and (= error-count 0) (= failure-count 0))
		  (format stream "all passed"))
		 (t
		  (format stream "~[~:;~:*~:d failure~:p~]" 
			  failure-count)
		  (when (and (> error-count 0) (> failure-count 0))
		    (format stream ", "))
		  (format stream  "~[~:;~:*~a error~:p~]" 
			  error-count)))))
	 (let ((problem (getf datum :problem)))
	   (cond ((typep problem 'test-failure)
		  (format stream "~&failure" ))
		 ((typep problem 'test-error)
		  (format stream "~&error"))
		 (t
		  (format stream "~&~a" test-name)
		  (let ((seconds (getf datum :seconds))
			(conses (getf datum :conses)))
		    (when seconds 
		      (format stream "~15,3f" seconds))
		    (when conses 
		      (format stream "~15:d" conses)))))))))

  
;;;;;

(defmethod summarize-test-result (result stream (format (eql :save)))
  (flet ((add-property (name)
	   (when (slot-boundp result name)
	     (format stream "~&\(~s ~a\)" 
		     (intern (symbol-name name) :keyword)
		     (slot-value result name)))))
    (format stream "\(~%")
    (add-property 'results-for)
    (format stream "~&\(:date-time ~a\)" (get-universal-time))
    (add-property 'real-start-time-universal)
    (add-property 'start-time-universal)
    (add-property 'end-time-universal)
    (add-property 'real-end-time-universal)
    (format stream "~&\(:tests-run ")
    (loop for (suite name data) in
	 (copy-list (tests-run result)) do
	 (write-log-test format suite name data :stream stream))
    (format stream "~&\)")
    (format stream "~&\)")))

#+(or)
(progn
  (setf (test-result-property *test-result* :if-exists) :supersede)
  (test-result-report *test-result* #p"/tmp/report.save" :save))

(defun ensure-symbol (thing)
  (etypecase thing
    (symbol thing)
    (string (intern thing))))

;;;;

(defun write-log-header (stream result args)
  (append-to-report (out stream)
    (format out "~&\(")
    (out :results-for (results-for result))
    (out :arguments args)
    (out :features (copy-list *features*))
    (out :datetime (get-universal-time))
    (loop for hook in *log-header-hooks* do
	 (funcall hook out result))
    (format out "~&\)~%")))

(defun write-log-footer (stream result)
  (append-to-report (out stream)
    (format out "~&\(")
    (out :test-case-count (length (tests-run result)))
    (out :test-suite-count (length (suites-run result)))
    (out :failure-count (length (failures result)))
    (out :error-count (length (errors result)))
    (out :expected-failure-count (length (expected-failures result)))
    (out :expected-error-count (length (expected-errors result)))
    (out :skipped-testsuites-count (length (skipped-testsuites result)))
    (out :skipped-test-cases-count (length (skipped-test-cases result)))
    (out :start-time-universal (start-time-universal result))
    (when (slot-boundp result 'end-time-universal)
      (out :end-time-universal (end-time-universal result)))
    (out :errors (collect-testsuite-summary-for-log result :errors))
    (out :failures (collect-testsuite-summary-for-log result :failures))
    (out :expected-errors
	 (collect-testsuite-summary-for-log result :expected-errors))
    (out :expected-failures 
	 (collect-testsuite-summary-for-log result :expected-failures))
    (out :skipped-testsuites 
	 (collect-testsuite-summary-for-log result :skipped-testsuites))
    (out :skipped-test-cases	 
	 (collect-testsuite-summary-for-log result :skipped-test-cases))
    (loop for hook in *log-footer-hooks* do
	 (funcall hook out result))
    (format out "~&\)~%")))

(defmethod write-log-test :around
    (format suite-name test-case-name data &key stream)
  (append-to-report (out stream)
    (call-next-method format suite-name test-case-name data :stream out)))

(defmethod write-log-test 
    ((format (eql :save)) suite-name test-case-name data
     &key (stream *standard-output*))
  (labels ((out (key value)
	     (when value
	       (format stream "~&\(~s . ~s\)" key value)))
	   (write-datum (name &key (source data))
	     (let* ((key (form-keyword name))
		    (value (getf source key)))
	       (out key value))))
    (format stream "~&\(~%")
    (out :suite suite-name)
    (out :name test-case-name)
    ;; FIXME - we could make these extensible
    (write-datum 'start-time-universal)
    (write-datum 'end-time-universal)
    (write-datum 'result)
    (write-datum 'seconds)
    (write-datum 'conses)
    (let ((properties (getf data :properties)))
      (loop for key in properties by #'cddr
	 for value in (rest properties) by #'cddr do
	 (out key value)))
    (cond ((getf data :problem)
	   (let ((problem (getf data :problem)))
	     (out :problem-kind (test-problem-kind problem))
	     (out :problem-step (test-step problem))
	     (out :problem-condition 
		  (let ((*print-readably* nil))
		    (format nil "~s" (test-condition problem))))
	     (out :problem-condition-description 
		  (format nil "~a" (test-condition problem)))
	     (when (slot-exists-p problem 'backtrace)
	       (out :problem-backtrace (backtrace problem)))))
	  (t
	   (out :result t)))
    (loop for hook in *log-detail-hooks* do
	 (funcall hook stream data))
    (format stream "\)~%")))

;;;;

(defun collect-testsuite-summary-for-log (result kind)
  (let ((list (slot-value result (intern (symbol-name kind) 
					 (find-package :lift)))))
    (flet ((encode-symbol (symbol)
	     (cons (symbol-name symbol) 
		   (package-name (symbol-package symbol)))))
      (mapcar (lambda (glitch)
		(if (test-method glitch)
		    (list (encode-symbol (testsuite glitch))
			  (encode-symbol (test-method glitch)))
		    (encode-symbol (testsuite glitch))))
	      list))))

#+(or)
(collect-testsuite-summary-for-log lift:*test-result* :skipped-testsuites)

;;;;;



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
	 (or *profiling-threshold* 0.01)))
    (prof:show-flat-profile :stream output)))

#+allegro
(defun show-call-graph (output)
  (let ((prof:*significance-threshold* 
	 (or *profiling-threshold* 0.01)))
    (prof:show-call-graph :stream output)))

#+allegro
(defun show-call-counts (output)
  (format output "~%~%Call counts~%")
  (let ((*standard-output* output))
    (prof:show-call-counts)))


#-allegro
(defun show-flat-profile (output)
  (format output "~%~%Flat profile: unavailable for this Lisp~%"))

#-allegro
(defun show-call-graph (output)
  (format output "~%~%Call graph: unavailable for this Lisp~%"))

#-allegro
(defun show-call-counts (output)
  (format output "~%~%Call counts: unavailable for this Lisp~%"))

#+allegro
(defun with-profile-report-fn 
    (name style fn body &key
     (log-name *benchmark-log-path*)
     (count-calls-p *count-calls-p*)
     (timeout nil))
  (assert (member style '(nil :time :space :count-only)))
  (when style
    (cancel-current-profile :force? t))
  (let* ((seconds 0.0) (conses 0) 
	 error
	 results
	 (profile-fn (make-profiled-function fn)))
    (unwind-protect
	 (multiple-value-bind (result measures errorp)
	     (while-measuring (t measure-seconds measure-space)
	       (handler-bind
		   ((timeout-error (lambda (_) (declare (ignore _))))
		    (error (lambda (c) (error c))))
		 (with-timeout (timeout)
		   (funcall profile-fn style count-calls-p))))
	   (setf seconds (first measures) conses (second measures) 
		 results result error errorp))
      ;; cleanup / ensure we get report
      (generate-profile-log-entry log-name name seconds conses results error)
      (when (and style (> (current-profile-sample-count) 0))
	(let ((pathname (unique-filename
			 (merge-pathnames
			  (make-pathname 
			   :type "prof"
			   :name (format nil "~a-~a-" name style))
			  log-name))))
	  (write-profile-report pathname name style body
				seconds conses error count-calls-p))))
    (values-list (if (atom results) (list results) results))))

(defun write-profile-report (pathname name style body seconds conses
			     error count-calls-p)
  (format t "~&Profiling output being sent to ~a" pathname)
  (with-open-file (output pathname
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :append)
    (format output "~&Profile data for ~a" name)
    (format output "~&Date: ~a" (date-stamp :include-time? t))
    (summarize-test-environment nil output nil)
    (format output "~&  Total time: ~,2F; Total space: ~:d \(~:*~d\)"
	    seconds conses)
    (format output "~%~%")
    (when error
      (format output "~&Error occurred during profiling: ~a~%~%" error))
    (let ((*standard-output* output))
      (when *current-test* 
	(write-profile-information *current-test*)))
    (when body
      (format output "~&Profiling: ~%")
      (let ((*print-length* 10)
	    (*print-level* 10))
	(dolist (form body)
	  (pprint form output)))
      (format output "~%~%"))
    (when (or (eq :time style)
	      (eq :space style))
      (show-flat-profile output)
      (show-call-graph output)
      (when count-calls-p
	(show-call-counts output)))
    #+allegro
    (when *functions-to-profile*
      (loop for thing in *functions-to-profile* do
	   (let ((*standard-output* output)
		 (*print-readably* nil))
	     (handler-case 
		 (cond ((thing-names-generic-function-p thing)
			(format output "~%~%Disassemble generic-function ~s:~%"
				thing)
			(prof:disassemble-profile thing)
			(mapc 
			 (lambda (m)
			   (format t "~2%~a~%"
				   (make-string 60 :initial-element #\-))
			   (format t "~&Method: ~a~2%" m)
			   (prof:disassemble-profile (clos:method-function m)))
			 (clos:generic-function-methods 
			  (symbol-function thing))))
		       (t
			(format output "~%~%Disassemble function ~s:~%"
				thing)
			(prof:disassemble-profile thing)))
	       (error (c)
		 (format 
		  output "~2%Error ~a while trying to disassemble-profile ~s~2%"
		  c thing))))))))

;; stolen from cl-markdown and modified
(defun thing-names-generic-function-p (thing)
  (and (symbolp thing)
       (fboundp thing)
       (typep (symbol-function thing) 'standard-generic-function)))

(defmethod save-configuration-file ((result test-result) destination)
  (with-open-file (stream destination
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (html-header 
     stream 
     "test configuration file"
     (test-result-property result :style-sheet))
    (format stream "~&<h1>Test configuration</h1>~%")
    (format stream "~&<pre>~%")
    (save-configuration-file (results-for result) stream)
    (format stream "~&</pre>~%")
    (html-footer stream)))

(defmethod save-configuration-file ((pathname t) stream)
    (with-open-file (*current-configuration-stream* pathname
						    :direction :input
						    :if-does-not-exist :error)
      (let ((form nil))
	(loop while (not (eq (setf form (read *current-configuration-stream* 
					      nil :eof nil)) :eof))
	   do
	   ;; special handling for include
	   (cond ((and (consp form) (eql (first form) :include))
		  (destructuring-bind (name &rest args)
		      form
		    (declare (ignore name))
		    (format stream "~&~%;; begin - include ~a~%" 
			    (first args))
		    (save-configuration-file
		     (merge-pathnames (ensure-string (first args))
				      *current-configuration-stream*)
		     stream)
		    (format stream "~&;; end - include ~a~%~%"
			    (first args))))
		 (t
		  (print form stream)))))))

(defun build-issues-report (result kind stream)
  (format stream
	  "~&<h2><a href=\"~a.html\">Test ~a summary</a></h2>~%" 
	  kind kind)
  (with-open-file (out (merge-pathnames (format nil "~a.html" kind)
					stream)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (html-header 
     out 
     (format nil "~a summary" kind)
     (test-result-property result :style-sheet))
    (format out "~&<h1>~a summary</h1>~%" kind)
    (loop for (string . issues) in (build-issues-list result kind) do
       (format out "~%~%<h2>~a</h2>~%~%" string)
       (format out "~&<ul>~%")	 
       (loop for issue in issues do
	    (destructuring-bind (_1 suite name _2) issue
	      (declare (ignore _1 _2))
	      (format out "~&<li><span>~a</span> <a href=\"~a\"><span>~a</span></a></li>~&" 
		      suite (details-link stream suite name) name)))
       (format out "~&</ul>~%"))
    (html-footer out)))

(defun test-case-skipped-p (result suite-name case-name)
  (or (find suite-name (skipped-testsuites result))
      (find-if (lambda (couplet)
		 (and (eq (first couplet) suite-name)
		      (eq (second couplet) case-name)))
	       (skipped-test-cases result))))

;;;; brief

(defmethod start-report-output (result stream (format (eql :brief)))
  (format stream "~&Test ~a" 
	  (or (test-result-property result :title) "report")))

(defmethod summarize-test-result (result stream (format (eql :brief)))
  (format stream"~&Test results for: ~a~%"
	  (results-for result)))

(defmethod summarize-test-environment (result stream (format (eql :brief)))
  (format stream "~&Lisp: ~a" (lisp-version-string))
  (format stream ", Machine: ~a ~a ~a, Date: ~a"  
	  (machine-type) (machine-version) (machine-instance)
	  (date-stamp :include-time? t))
  (let ((complete-success? (and (null (errors result))
                                (null (failures result))))) 
    (cond (complete-success?
	   (format stream"~&~A Successful test~:P"
		   (length (tests-run result))))
	  (t
	   (format stream "~&~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]"
		   (length (tests-run result))
		   (length (failures result))
		   (length (errors result)))))
    (when (expected-errors result)
      (format stream ", ~a expected error~:p" (length (expected-errors result))))
    (when (expected-failures result)
      (format stream ", ~a expected failure~:p"
	      (length (expected-failures result))))
    (when (skipped-test-cases result)
      (format stream ", ~a skipped case~:p"
	      (length (skipped-test-cases result))))))

(defmethod summarize-test-problems (result stream  (format (eql :brief)))
  (loop for (tag cases) in
       `((:skipped ,(skipped-test-cases result))
	 (:expected-failures ,(expected-failures result))
	 (:expected-errors ,(expected-errors result))
	 (:failures ,(failures result))
	 (:errors  ,(errors result))) do
       (when cases
	 (format stream "~&~a~%" tag)
	 (report-tests-by-suite 
	  format  
	  (mapcar (lambda (problem)
		    `(,(testsuite problem)
		       ,(test-method problem)
		       (:problem ,problem)))
		  cases)
	  stream))))

(defmethod report-test-suite-by-suite 
    ((format (eql :brief)) stream remaining current-suite suite)
  (declare (ignore stream remaining current-suite suite))
  )

(defmethod report-test-case-by-suite 
    ((format (eql :brief)) stream suite test-name datum)
  (declare (ignore datum))
  (format stream "~&  :suite '~a :name '~a~%" suite test-name))

(defmethod finish-report-tests-by-suite
    ((format (eql :brief)) stream current-suite)
  (declare (ignore stream current-suite))
  )

 