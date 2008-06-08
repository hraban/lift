(in-package #:lift)

(defgeneric find-testsuite (suite)
  )

(defgeneric find-test-case (suite name)
  )

(defmethod find-test-cases (name)
  )

;;;;;
;; some introspection

(defun liftpropos (name &key (include-cases? nil))
  (declare (ignore include-cases?))
  (let ((result nil)
	(real-name (etypecase name
		     (string name)
		     (symbol (symbol-name name)))))
    (map-testsuites
     (lambda (suite level)
       (declare (ignore level))
       (let ((suite-name (symbol-name (class-name suite))))
	 (when (search real-name suite-name :test #'char-equal)
	   (push suite-name result))))
     'test-mixin)
    (sort result #'string-lessp)))

(defun map-testsuites (fn start-at)
  (let ((visited (make-hash-table)))
    (labels ((do-it (suite level)
	       (unless (gethash suite visited)
		 (setf (gethash suite visited) t)
		 (funcall fn suite level)
		 (loop for subclass in (subclasses suite :proper? t) do
		      (do-it subclass (1+ level))))))
    (do-it (find-class (find-testsuite start-at) nil) 0))))

(defun testsuites (&optional (start-at 'test-mixin))
  "Returns a list of testsuite classes. The optional parameter provides
control over where in the test hierarchy the search begins."
  (let ((result nil))
    (map-testsuites (lambda (suite level)
		      (declare (ignore level))
		      (push suite result))
		    start-at)
    (nreverse result)))

(defun print-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Prints all of the defined test classes from :start-at on down." 
  (map-testsuites
   (lambda (suite level)
     (let ((indent (coerce (make-list (* level 3) :initial-element #\Space)
			   'string))
	   (name (class-name suite)))
       (format stream "~&~a~s (~:d)" 
	       indent
	       name
	       (length (testsuite-methods name)))
       (when include-cases?
	 (loop for method-name in (testsuite-tests name) do
	      (format stream "~&~a  ~a" indent method-name)))))
   start-at))
     
(defun list-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Lists all of the defined test classes from :start-at on down." 
  (mapc (lambda (subclass)
	  (let ((subclass-name (class-name subclass)))
	    (format stream "~&~s (~:d)" 
		    subclass-name
		    (length (testsuite-methods subclass-name)))
	    (when include-cases?
	      (loop for method-name in (testsuite-tests subclass-name) do
		   (format stream "~&  ~a" method-name)))))
        (testsuites start-at))
  (values))

(defun testsuite-test-count (testsuite)
  (or (and *testsuite-test-count* 
           (prog1 *testsuite-test-count* (incf *testsuite-test-count*))) 
      (length (testsuite-methods testsuite))))

(defmethod find-testsuite ((suite symbol))
  (or (testsuite-p suite)
      (find-testsuite (symbol-name suite))))

(defmethod find-testsuite ((suite-name string))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol suite-name p))
				      (find-class temp nil)
				      (subtypep temp 'test-mixin)) collect
			    temp))))
    (cond ((null possibilities) 
	   (error 'testsuite-not-defined :testsuite-name suite-name))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (error "There are several test suites named ~s: they are ~{~s~^, ~}"
		  suite-name possibilities)))))
			     
(defun test-case-p (suite-class name)
  (find-method #'lift-test nil `(,suite-class (eql ,name)) nil)) 

#+(or)
(test-case-p 
 (find-class (find-testsuite 'test-cluster-indexing-locally) nil)
 'db.agraph.tests::index-them)

#+(or)
(find-test-case (find-class (find-testsuite 'test-cluster-indexing-locally))
		'index-themxx)

(defmethod find-test-case ((suite symbol) name)
  (find-test-case (find-class (find-testsuite suite)) name)) 

(defmethod find-test-case ((suite null) name)
  (find-test-cases name)) 

(defmethod find-test-case ((suite test-mixin) name)
  (find-test-case (class-of suite) name))

(defmethod find-test-case ((suite-class standard-class) (name symbol))
  (or (and (test-case-p suite-class name) name)
      (find-test-case suite-class (symbol-name name))))

(defmethod find-test-case ((suite test-mixin) (name string))
  (find-test-case (class-of suite) name))

(defmethod find-test-case ((suite-class standard-class) (name string))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol name p))
				      (test-case-p suite-class temp)) collect
			    temp))))
    (cond ((null possibilities) 
	   (error 'test-case-not-defined 
		  :testsuite-name suite-class :test-case-name name))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (error "There are several test cases of ~s named ~s: they are ~{~s~^, ~}"
		  suite-class name possibilities)))))
			     
(defmethod find-test-cases ((name symbol))
  (find-test-cases (symbol-name name)))

(defmethod find-test-cases ((name string))
  (let ((result nil))
    (dolist (testsuite (testsuites))
      (let* ((suitename (class-name testsuite))
	     (testname (find-symbol name (symbol-package suitename))))
	(when (and testname 
		   (test-case-p testsuite testname))
	  (push (cons suitename testname) result))))
    result))

(defun last-test-status ()
  (cond ((typep *test-result* 'test-result)
	 (cond ((and (null (errors *test-result*))
		     (null (failures *test-result*)))
		:success)
	       ((and (errors *test-result*)
		     (failures *test-result*))
		:errors-and-failures)
	       ((errors *test-result*)
		:errors)
	       ((failures *test-result*)
		:failures)))
	(t
	 nil)))

(defun suite-tested-p (suite &key (result *test-result*))
  (and result
       (typep *test-result* 'test-result)
       (slot-exists-p result 'suites-run)
       (slot-boundp result 'suites-run)
       (consp (suites-run result))
       (find suite (suites-run result))))
