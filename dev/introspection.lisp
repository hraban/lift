(in-package #:lift)

(defgeneric find-testsuite (suite &key errorp)
  (:documentation "Search for a testsuite named `suite`. 

The search is conducted across all packages so `suite` can be 
a symbol in any package. I.e., find-testsuite looks for testsuite 
classes whose symbol-name is string= to `suite`. If `errorp` is 
true, then find-testsuite can raise two possible errors:

 * If more than one matching testsuite is found, 
then an error of type `testsuite-ambiguous` will be raised. 
 * If no matching testsuites are found, then an error of type 
`testsuite-not-defined` will be raised. 

The default for `errorp` is nil."))

(defgeneric find-test-case (suite name &key errorp)
  (:documentation "Search for a test-case named `name` in a 
testsuite named `suite`. 

The search is conducted across all packages so `suite` and `name` 
can be symbols in any package. I.e., find-test-case looks for a
testsuites and test-cases whose symbol-names are string= to
`suite` and `name`. If `errorp` is 
true, then find-test-case can raise two possible errors:

 * If more than one matching test-case is found, 
then an error of type `test-case-ambiguous` will be raised. 
 * If no matching test-cases are found, then an error of type 
`test-case-not-defined` will be raised. 

The default for `errorp` is nil. If `suite` is nil, then
find-test-case will search for matching test-cases across 
all suites. This is equivalent to the behavior of [find-test-cases][]."))

(defgeneric find-test-cases (name &key errorp)
  )

;;;;;
;; some introspection

(defmethod testsuite-p ((classname symbol))
  (let ((class (find-class classname nil)))
    (handler-case
      (and class
           (typep (allocate-instance class) 'test-mixin)
	   classname)
      (error (c) (declare (ignore c)) (values nil)))))

(defmethod testsuite-p ((object standard-object))
  (testsuite-p (class-name (class-of object))))

(defmethod testsuite-p ((class standard-class))
  (testsuite-p (class-name class)))

(defmethod testsuite-methods ((classname symbol))
  (testsuite-tests classname))

(defmethod testsuite-methods ((test test-mixin))
  (testsuite-methods (class-name (class-of test))))

(defmethod testsuite-methods ((test standard-class))
  (testsuite-methods (class-name test)))

(defun liftpropos (string &key (include-cases? nil) (start-at 'test-mixin))
  "Returns a list of testsuites whose name contains `string`."
  (let ((result nil)
	(name-as-string (strip-whitespace (ensure-string string))))
    (flet ((add-if-match (suite-name &optional (to-add suite-name))
	     (when (search name-as-string (ensure-string suite-name)
			   :test #'char-equal)
	       (push to-add result))))
      (map-testsuites
       (lambda (suite-name level)
	 (declare (ignore level))
	 (add-if-match suite-name)
	 (when include-cases?
	   (loop for method-name in (testsuite-tests suite-name) do
		(add-if-match 
		 method-name (cons suite-name method-name)))))
       start-at))
    (sort result #'string-lessp :key (lambda (it)
				       (typecase it
					 (atom it)
					 (cons (cdr it)))))))

(defun map-testsuites (fn start-at)
  "Call `fn` with each suite name starting at `start-at`

`fn` should be a function of two arguments. It will called with
a testsuite name and the `level` of the suite in the class hierarchy."
  (let ((visited (make-hash-table)))
    (labels ((do-it (suite level)
	       (unless (gethash suite visited)
		 (setf (gethash suite visited) t)
		 (funcall fn (class-name suite) level)
		 (loop for subclass in (direct-subclasses suite) do
		      (do-it subclass (1+ level))))))
    (do-it (find-class (find-testsuite start-at :errorp t) nil) 0))))

(defun collect-testsuites (start-at &key filter transform)
  (let ((filter (when filter (ensure-function filter)))
	(transform (when transform (ensure-function transform)))
	(result nil))
    (map-testsuites
     (lambda (suite level)
       (when (or (null filter) (funcall filter suite level))
	 (push (if transform (funcall transform suite level) suite) result)))
     start-at)
    (nreverse result)))

(defun map-test-cases (fn start-at)
  "Call `fn` with each test case for suites starting at `start-at`

`fn` should be a function of three arguments. It will called with
a testsuite name, a test-case name and the `level` of the suite
in the class hierarchy."  
  (map-testsuites 
   (lambda (suite-name level)
     ;; this suite / suite-name thing is messy
     (loop for method-name in (testsuite-tests suite-name) do
	  (funcall fn suite-name method-name level)))
   start-at))

(defun collect-test-cases (&optional (start-at 'test-mixin))
  "Returns a list of cons-pairs of testsuites and testcases. The optional
parameter provides control over where in the test hierarchy the search
begins."
  (let ((result nil))
    (map-test-cases (lambda (suite name level)
		      (declare (ignore level))
		      (push (cons suite name) result))
		    start-at)
    (nreverse result)))

;; deprecate
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
	   (name suite))
       (format stream "~&~a~s (~:d)" 
	       indent
	       name
	       (length (testsuite-methods name)))
       (when include-cases?
	 (loop for method-name in (testsuite-tests name) do
	      (format stream "~&~a  ~a" indent method-name)))))
   start-at))

;; deprecate
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

(defmethod test-case-count (testsuite)
  (let ((result 0))
    (map-testsuites (lambda (suite level)
		      (declare (ignore level))
		      (incf result (length (testsuite-methods suite))))
		    testsuite)
    result))

(defun testsuite-test-count (testsuite)
  (or (and *testsuite-test-count* 
           (prog1 *testsuite-test-count* (incf *testsuite-test-count*))) 
      (length (testsuite-methods testsuite))))

(defmethod find-testsuite ((suite test-mixin) &key (errorp nil))
  (declare (ignore errorp))
  (class-name (class-of suite)))

(defmethod find-testsuite ((suite symbol) &key (errorp nil))
  (or (testsuite-p suite)
      (find-testsuite (symbol-name suite) :errorp errorp)))

(defmethod find-testsuite ((suite-name string) &key (errorp nil))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol suite-name p))
				      (find-class temp nil)
				      (subtypep temp 'test-mixin)) collect
			    temp))))
    (cond ((null possibilities) 
	   (when errorp
	     (error 'testsuite-not-defined :testsuite-name suite-name)))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (if errorp
	     (error 'testsuite-ambiguous
		    :testsuite-name suite-name 
		    :possible-matches possibilities))
	   possibilities))))
	
(defun test-case-p (suite-class name)
  (gethash name (test-name->methods (class-name suite-class))))

#+(or)
;; old
(defun test-case-p (suite-class name)
  (find-method #'lift-test nil `(,suite-class (eql ,name)) nil)) 

#+(or)
(test-case-p 
 (find-class (find-testsuite 'test-cluster-indexing-locally) nil)
 'db.agraph.tests::index-them)

#+(or)
(find-test-case (find-class (find-testsuite 'test-cluster-indexing-locally))
		'index-themxx)

(defmethod find-test-case ((suite symbol) name &key (errorp nil))
  (find-test-case 
   (find-class (find-testsuite suite :errorp errorp)) name :errorp errorp)) 

(defmethod find-test-case ((suite null) name &key (errorp nil))
  (find-test-cases name :errorp errorp)) 

(defmethod find-test-case ((suite test-mixin) name &key (errorp nil))
  (find-test-case (class-of suite) name :errorp errorp))

(defmethod find-test-case ((suite-class standard-class) (name symbol)
			    &key (errorp nil))
  (or (and (test-case-p suite-class name) name)
      (find-test-case suite-class (symbol-name name) :errorp errorp)))

(defmethod find-test-case ((suite test-mixin) (name string)
			   &key (errorp nil))
  (find-test-case (class-of suite) name :errorp errorp))

(defmethod find-test-case ((suite-class standard-class) (name string)
			    &key (errorp nil))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol name p))
				      (test-case-p suite-class temp)) collect
			    temp))))
    (cond ((null possibilities) 
	   (when errorp
	     (error 'test-case-not-defined 
		    :testsuite-name suite-class :test-case-name name)))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (when errorp
	     (error 'test-case-ambiguous
		    :testsuite-name suite-class
		    :test-case-name name
		    :possible-matches possibilities))))))
			     
(defmethod find-test-cases ((name symbol) &key (errorp nil))
  (find-test-cases (symbol-name name) :errorp errorp))

(defmethod find-test-cases ((name string) &key (errorp nil))
  (let ((result nil))
    (dolist (testsuite (testsuites))
      (let* ((suitename (class-name testsuite))
	     (testname (find-symbol name (symbol-package suitename))))
	(when (and testname 
		   (test-case-p testsuite testname))
	  (push (cons suitename testname) result))))
    (unless result
      (when errorp
	(error "not test-cases found")))
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

(defun test-case-tested-p (suite name &key (result *test-result*))
  (let ((suite-name (find-testsuite suite)))
    (and result
	 (typep *test-result* 'test-result)
	 (slot-exists-p result 'tests-run)
	 (slot-boundp result 'suites-run)
	 (third (find-if (lambda (datum)
			   (and (eq (first datum) suite-name)
				(eq (second datum) name)))
			 (tests-run result))))))
  
(defun suite-tested-p (suite &key (result *test-result*))
  (let ((suite (find-testsuite suite)))
    (and result
	 (typep *test-result* 'test-result)
	 (slot-exists-p result 'suites-run)
	 (slot-boundp result 'suites-run)
	 (consp (suites-run result))
	 (find suite (suites-run result)))))


(defun test-results (&key (result *test-result*) (failures? t)
		     (errors? t) (successes? nil)
		     (expected-failures? t) (expected-errors? t)
		     (skipped-testsuites? nil) (skipped-test-cases? nil))
  (let ((acc nil))
    (flet ((gather (list kind process?)
            (when list
              (push (cons
                     kind 
                     (if process?
                         (loop for item in list collect
                              (list (testsuite item)
                                    (test-method item) item))
                         list)) acc))))
      (when errors? 
       (gather (errors result) :errors t))
      (when failures? 
       (gather (testsuite-failures result) :failures t))
      (when expected-errors? 
       (gather (expected-errors result) :expected-errors t))
      (when expected-failures? 
       (gather (expected-failures result) :expected-failures t))
      (when skipped-testsuites?
       (gather (skipped-testsuites result) :skipped-testsuites t))
      (when skipped-test-cases?
       (gather (skipped-test-cases result) :skipped-test-cases t))
      (when successes?
       (gather (loop for (suite test-case data) in (tests-run result) 
                    unless (getf data :problem) collect (list suite test-case))
               :successes nil))
      (nreverse acc))))

(defun test-successes (&key (result *test-result*))
  (cdr (first (test-results :result result 
                           :successes? t
                           :errors? nil :expected-errors? nil
                           :failures? nil :expected-failures? nil))))

(defun massage-condition-string (triple)
  (destructuring-bind (suite name condition)
      triple
    (declare (ignore suite name))
    (let ((string (princ-to-string (test-condition condition))))
      (setf string (rewrite-unreadables string))
      (let ((length (length string)))
	(subseq string 0 (min length 200))))))

(defun rewrite-unreadables (string)
  #+allegro
  (excl:replace-regexp string "#<\\([^ ]*\\) @ #x[0-9a-zA-Z]*>" "#<\\1>")
  #-allegro
  string)

;; could build up a list here too
;; expensive, don't keep calling `massage-condition-string`
(defun build-issues-list (result kind)
  (let* ((args (list :failures? nil :errors? nil :expected-failures? nil
		     :expected-errors? nil :skipped-testsuites? nil 
		     :skipped-test-cases? nil)))
    (ecase kind
      (:errors (setf (getf args :errors?) t))
      (:failures (setf (getf args :failures?) t))
      (:expected-failures (setf (getf args :expected-failures?) t))
      (:expected-errors (setf (getf args :expected-errors?) t))
      (:skipped-testsuites (setf (getf args :skipped-testsuites?) t))
      (:skipped-test-cases (setf (getf args :skipped-test-cases?) t)))
    (let ((tests (mapcar (lambda (triple)
			   (list* (massage-condition-string triple) triple))
			 (rest (first (apply #'test-results 
					     :result result args)))))
	  (result nil)
	  (sub-result nil)
	  (last-string ""))
      (flet ((grab (sub-result)
	       (when sub-result
		 (push (cons last-string sub-result) result))))
	(loop for datum in 
	     (sort tests 'string-lessp :key 'first) do
	     (destructuring-bind (string _0 _1 _2)
		 datum
	       (declare (ignore _0 _1 _2))
	       (unless (string= last-string string)
		 (grab sub-result)
		 (setf last-string string
		       sub-result nil))
	       (push datum sub-result)))
	(grab sub-result)
	(nreverse result)))))

(defun report-issues (result kind)
  (loop for (string . issues) in (build-issues-list result kind) do
       (format t "~%~%~a~%~%" string)
       (loop for issue in issues do
	    (destructuring-bind (_1 suite name _2) issue
	      (declare (ignore _1 _2))
	      (format t "~&   ~a ~a~&" suite name)))))

#+(or)
(db.agraph::with-new-file (*standard-output* "errors.txt")
  (report-issues tr :errors))

#+(or)
(db.agraph::with-new-file (*standard-output* "failures.txt")
  (report-issues tr :failures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; experimental

(defun divide-cases (cases)
  (let ((result nil)
	(sub-result nil)
	(rest cases)
	(current-suite nil)
	(target-size (floor (length cases) 2)))
    (flet ((add-group (suite)
	     (when sub-result
	       (setf result (nconc result sub-result)))
	     (setf current-suite suite
		   sub-result nil)
	     (when (>= (length result) target-size)
	       (return-from divide-cases (values result rest)))))
      (loop for (suite . name) in cases do
	   (unless (eq current-suite suite)
	     (add-group suite))
	   (pop rest)
	   (push (cons suite name) sub-result))
      (add-group sub-result))))

#|
(divide-cases 
 '((1 . a) (1 . b) (1 . c) (2 . a) (2 . b) (3 . a) (3 . b) (3 . c) (3 .d)))

(divide-cases 
 '((1 . a) (1 . b) (1 . c) (2 . a) (2 . b) (3 . a) (3 . b) (3 . c)))

(divide-cases 
 '((1 . a) (1 . b) (1 . c) (2 . a) (2 . b) (3 . a) (3 . b) (3 . c) (3 . d) (3 . e)))
|#

(defun suites-in-cases (cases)
  (remove-duplicates (mapcar #'car cases)))

(defun suites-in-portion (cases path)
  (loop for part in path do
       (setf cases (ecase part
		     ((:top :t) (divide-cases cases))
		     ((:bot :b :bottom) (nth-value 1 (divide-cases cases))))))
  (suites-in-cases cases))

(defun suites-in-portions (cases paths)
  ;;?? nconc
  (loop for path in paths append
       (suites-in-portion cases path)))

#+(or)
(lift:run-tests
 :suite (lift::suites-in-portion 
	 (lift::collect-test-cases 'db.agraph.tests)
	 '(:b)))
