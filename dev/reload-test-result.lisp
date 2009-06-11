(in-package #:lift)

#|
The problem is that we need to able to go back and forth
between lift's internal representation and the file representation
and these representations weren't designed for it. So it'll take 
more munging that I'd hoped.
|#

#+(or)
(asdf:system-relative-pathname 
 'log5 "test-results/allegro-8.1m-macosx-x86/summary-2009-06-04-1.sav")

(defun reload-test-result (pathname)
  (let ((result (make-test-result pathname :multiple))
	head tail tests)
    (metatilities:map-forms 
     pathname
     (lambda (f)
       (cond (head
	      (setf tail f)
	      (push f tests))
	     (t
	      (setf head f)))))
    (if (find :test-case-count tail :key #'first)
      (pop tests)
      (setf tail nil))
    (bind:bind (((:alist (results-for :results-for)
			 (arguments :arguments)
			 (features :features)
			 (datetime :datetime))
		 head)
		((:alist (test-case-count :test-case-count)
			 (test-suite-count :test-suite-count)
			 (failure-count :failure-count)
			 (error-count :error-count)
			 (expected-failure-count :expected-failure-count)
			 (expected-error-count :expected-error-count)
			 (start-time-universal :start-time-universal)
			 (end-time-universal :end-time-universal) 
			 (failures :failures))
		 tail))
      (setf (slot-value result 'real-start-time-universal) 
	    start-time-universal
	    (slot-value result 'start-time-universal) 
	    start-time-universal
	    (slot-value result 'real-end-time-universal) 
	    end-time-universal
	    (slot-value result 'end-time-universal) 
	    end-time-universal)
      (loop for test in tests do
	   (bind:bind (((:alist 
			 ((suite :suite) (name :name)
			  (start-time-universal :start-time-universal)
			  (end-time-universal :end-time-universal)
			  (seconds :seconds)
			  (conses :conses) (result :result) 
			  (problem-kind :problem-kind)
			  (problem-step :problem-step)
			  (problem-condition :problem-condition)
			  (problem-condition-description 
			   :problem-condition-description))) test))
	     `(,suite ,name 
		      (:end-time-universal 
		       ,end-time-universal
		       :end-time 
		       ,end-time-universal
		       :start-time-universal 
		       ,start-time-universal
		       :start-time 
		       ,start-time-universal
		       :conses ,conses
		       :seconds ,seconds 
		       ,@(reconstruct-problem test)))))

(defun problem-kind-to-problem-class (problem-kind)
  (let* ((problem-class-name (format nil "test-~(~a~)"
				     (subst #\- #\Space problem-kind)))
	 (problem-class (find-symbol problem-class-name)))
    (find-class problem-class nil)))

(defun reconstruct-problem (suite name test)
  (bind:bind (((:alist (problem-kind :problem-kind)) test))
    (when problem-kind
      (bind:bind (((:alist	    
		    (problem-step :problem-step)
		    (problem-condition :problem-condition)
		    (problem-condition-description 
		     :problem-condition-description)) test))
	(make-instance 
	 (problem-kind-to-problem-class problem-kind)
	 :testsuite suite
	 :test-method name
	 :test-condition 
	 (cons problem-condition problem-condition-description)
	 :test-step problem-step)))))

#+(or)
(reconstruct-problem 'a 'b 
'(
(:suite . log5-test::test-message-formatting)
(:name . log5-test::formatted-string-with-tilde)
(:start-time-universal . 3453139217)
(:end-time-universal . 3453139217)
(:problem-kind . "failure")
(:problem-step . :end-test)
(:problem-condition . "#<lift::ensure-not-same @ #x10af2f92>")
(:problem-condition-description . "Ensure-same: \"hello there, ~
how are you?
\" is not #<Function string=> to \"hello there, how are you?
\"")))

