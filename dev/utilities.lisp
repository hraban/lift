(in-package #:lift)

(defvar *lift-debug-output* *debug-io*
  "Messages from LIFT will be sent to this stream. It can set to nil or 
to an output stream. It defaults to *debug-io*.")

(defvar *test-print-testsuite-names* t
  "If true, LIFT will print the name of each test suite to *debug-io* before it begins to run the suite. See also: *test-print-test-case-names*.")

(defvar *test-print-test-case-names* nil
  "If true, LIFT will print the name of each test-case before it runs. See also: *test-print-testsuite-names*.")


;; stolen from metatilities
(defun form-symbol-in-package (package &rest names)
    "Finds or interns a symbol in package whose name is formed by concatenating the pretty printed representation of the names together."
    (with-standard-io-syntax 
      (let ((*package* package))
	(intern (format nil "~{~a~}" names)
		package))))
     
(defun form-symbol (&rest names)
    "Finds or interns a symbol in the current package whose name is formed by concatenating the pretty printed representation of the names together."
    (apply #'form-symbol-in-package *package* names))

(defun form-keyword (&rest names)
    "Finds or interns a symbol in the current package whose name is formed by concatenating the pretty printed representation of the names together."
    (apply #'form-symbol-in-package 
	   (load-time-value (find-package :keyword)) names))

;; borrowed from asdf
(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

(defun pathname-has-device-p (pathname)
  (and (or (stringp pathname) (pathnamep pathname))
       (not (member (pathname-device pathname) '(nil :unspecific)))))

(defun pathname-has-host-p (pathname)
  (and (or (stringp pathname) (pathnamep pathname))
       (not (member (pathname-host pathname) '(nil :unspecific)))))

(defun relative-pathname (relative-to pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf directory (copy-list directory)
	    (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory directory
		    )
     relative-to)))

(defun directory-pathname-p (p)
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))

(defun directory-p (name)
  (let ((truename (probe-file name)))
    (and truename (directory-pathname-p name))))

(defun containing-pathname (pathspec)
  "Return the containing pathname of the thing to which 
pathspac points. For example:

    > \(containing-directory \"/foo/bar/bis.temp\"\)
    \"/foo/bar/\"
    > \(containing-directory \"/foo/bar/\"\)
    \"/foo/\"
"
  (make-pathname
   :directory `(,@(butlast (pathname-directory pathspec)
			   (if (directory-pathname-p pathspec) 1 0)))
   :name nil
   :type nil
   :defaults pathspec))

;; FIXME -- abstract and merge with unique-directory
(defun unique-filename (pathname &optional (max-count 10000))
  (let ((date-part (date-stamp)))
    (loop repeat max-count
       for index from 1
	 for name = 
	 (merge-pathnames 
	  (make-pathname
	   :name (format nil "~a-~a-~d" 
			 (pathname-name pathname)
			 date-part index))
	  pathname) do
	 (unless (probe-file name)
	   (return-from unique-filename name)))
    (error "Unable to find unique pathname for ~a; there are already ~:d similar files" pathname max-count)))
	    
;; FIXME -- abstract and merge with unique-filename
(defun unique-directory (pathname)
  (setf pathname (merge-pathnames pathname))
  (let* ((date-part (date-stamp))
	 (last-directory (first (last (pathname-directory pathname))))
	 (base-pathname (containing-pathname pathname))
	 (base-name (pathname-name last-directory))
	 (base-type (pathname-type last-directory)))
    (or (loop repeat 10000
	   for index from 1
	   for name = 
	   (merge-pathnames 
	    (make-pathname
	     :name nil
	     :type nil
	     :directory `(:relative 
			  ,(format nil "~@[~a-~]~a-~d~@[.~a~]" 
				   base-name date-part index base-type)))
	    base-pathname) do
	   (unless
           #-clisp
           (probe-file name)
           #+clisp
           (ignore-errors
             (let ((directory-form (pathname-as-directory pathname)))
               (when (ext:probe-directory directory-form)
                 directory-form)))
	       (return name)))
	(error "Unable to find unique pathname for ~a" pathname))))

(defun date-stamp (&key (datetime (get-universal-time)) (include-time? nil) 
		   (include-date? t)
		   (time-delimiter #\-) (date-delimiter #\-) (date-time-separator #\T))
  (multiple-value-bind
	(second minute hour day month year day-of-the-week)
      (decode-universal-time datetime)
    (declare (ignore day-of-the-week))
    (let ((date-part (format nil "~d~@[~c~]~2,'0d~@[~c~]~2,'0d"
			     year date-delimiter month date-delimiter day))
	  (time-part (and include-time? 
			  (format nil "~2,'0d~@[~c~]~2,'0d~@[~c~]~2,'0d"
				  hour time-delimiter minute
				  time-delimiter second))))
      (format nil "~@[~a~]~@[~c~]~@[~a~]"
	      (and include-date? date-part)
	      (and include-date? include-time? date-time-separator)
	      (and include-time? time-part)))))

#-allegro
(defun format-test-time-for-log (test-time)
  (multiple-value-bind (ut fsecs) 
      (truncate test-time 1000)
    (declare (ignore fsecs))  
    (date-stamp :datetime ut :include-date? nil :include-time? t :time-delimiter #\:)))

#+allegro
(defun format-test-time-for-log (test-time)
  (multiple-value-bind (ut fsecs) 
      (truncate test-time 1000)
    (with-output-to-string (out) 
      (let* ((time
	      (excl:locale-print-time ut :fmt "%T" :stream nil)
	       #+no
	       (excl:locale-print-time ut :fmt "%Y-%m-%dT%T" :stream nil)))
	(format out "~a.~3,'0d" time fsecs)))))

#+(or)
(date-stamp :include-time? t)	

;;; ---------------------------------------------------------------------------
;;; shared stuff
;;; ---------------------------------------------------------------------------	

(defgeneric get-class (thing &key error?)
  (:documentation "Returns the class of thing or nil if the class cannot be found. Thing can be a class, an object representing a class or a symbol naming a class. Get-class is like find-class only not as particular.")
  (:method ((thing symbol) &key error?)
           (find-class thing error?))
  (:method ((thing standard-object) &key error?)
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing t) &key error?) 
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing class) &key error?)
           (declare (ignore error?))
           thing))

(defun direct-subclasses (thing)
  "Returns the immediate subclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-subclasses (get-class thing)))

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (direct-subclasses class)))))
      (do-it (get-class class) t))))

(defun subclasses (class &key (proper? t))
  "Returns all of the subclasses of the class including the class itself."
  (let ((result nil))
    (map-subclasses class (lambda (class)
                            (push class result))
                    :proper? proper?)
    (nreverse result)))

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class,
object or symbol naming a class. The list of classes returned is
'proper'; it does not include the class itself."
  (let ((result (class-precedence-list (get-class thing))))
    (if proper? (rest result) result)))

(defun class-slot-names (thing)
  (declare (ignorable thing))
  #+allegro
  (let ((class (get-class thing)))
    (if class
      (mapcar 'mop:slot-definition-name
	      (mop:class-slots (finalize-class-if-necessary class)))
      (progn
	(warn "class for ~a not found)" thing)
	nil))))

(defun finalize-class-if-necessary (thing)
  "Finalizes thing if necessary. Thing can be a class, object or
symbol naming a class. Returns the class of thing."
  (declare (ignorable thing))
  #+allegro
  (let ((class (get-class thing)))
    (unless (mop:class-finalized-p class)
      (mop:finalize-inheritance class))
    (values class)))

(declaim (inline length-1-list-p)) 
(defun length-1-list-p (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun parse-brief-slot (slot)
  (let* ((slot-spec 
	  (typecase slot
	    (symbol (list slot))
	    (list slot)
	    (t (error "Slot-spec must be a symbol or a list. `~s` is not." 
		      slot)))))
    (unless (null (cddr slot-spec))
      (error "Slot-spec must be a symbol or a list of length one or two. `~s` has too many elements." slot)) 
    `(,(first slot-spec) 
       :initarg ,(form-keyword (first slot-spec))
       :initform ,(when (second slot-spec) `,(second slot-spec)))))

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

(defun remove-leading-quote (list)
  "Removes the first quote from a list if one is there."
  (if (and (consp list) (eql (first list) 'quote))
    (first (rest list))
    list))

(defun cleanup-parsed-parameter (parameter)
  (if (length-1-list-p parameter)
    (first parameter)
    parameter))

(defun ensure-string (it)
  (etypecase it
    (string it)
    (symbol (symbol-name it))))

(defun ensure-function (thing)
  (typecase thing
    (function thing)
    (symbol (symbol-function thing))))

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

;;;;

(defun version-numbers (version &optional padded)
  "Returns a list of the version numbers in a #\. delimited string of
integers. E.g. (version-numbers \"2.2.1\") ==> (2 2 1). If the optional
`padded` parameter is included, the length of the returned list will be
right-padded with zeros so that it is of length padded (the list won't
be truncated if padded is smaller than the number of version digits in 
the string."
  (let ((result (mapcar 'safe-parse-integer (split version '(#\.)))))
    (if padded 
	(pad-version result padded)
	result)))

(defun canonical-versions-numbers (v-1 v-2)
  (let* ((v-1s (version-numbers v-1))
	 (v-2s (version-numbers v-2))
	 (max (max (length v-1s) (length v-2s))))
    (values (pad-version v-1s max) (pad-version v-2s max))))

(defun version= (v-1 v-2)
  (multiple-value-bind (v-1s v-2s)
      (canonical-versions-numbers v-1 v-2)
    (every (lambda (v1 v2) (= v1 v2))
	   v-1s v-2s)))

(defun version< (v-1 v-2)
  (multiple-value-bind (v-1s v-2s)
      (canonical-versions-numbers v-1 v-2)
    (loop for last1 = nil then v1
       for last2 = nil then v2
       for v1 in v-1s
       for v2 in v-2s 	 
       when (or (and (null last1) (> v1 v2))
		(and (not (null last1)) 
		     (= last1 last2)
		     (> v1 v2))) do
       (return-from version< nil)
       when (< v1 v2) do
       (return-from version< t)
       finally 
       ;; everything equal
       (when (and (not (null last1)) (= last1 last2))
	 (return-from version< nil)))
    t))

(defun pad-version (v size)
  (assert (<= (length v) size))
  (append v (make-list (- size (length v)) :initial-element 0)))

(defun version<= (v-1 v-2)
  (or (version< v-1 v-2)
      (version= v-1 v-2)))

(defun version> (v-1 v-2)
  (not (version<= v-1 v-2)))

(defun version>= (v-1 v-2)
  (not (version< v-1 v-2)))

(defun safe-parse-integer (string)
  (parse-integer string :junk-allowed t))

(defun split (string &optional (ws '(#\Space #\Tab)))
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
	(setf end (position-if #'is-ws string :start start))
	(push (subseq string start end) list)
	(incf words)
	(unless end (return list))
	(setf start (1+ end)))))))

;;; whitespace-p

(defparameter +whitespace-characters+
  (list #\Space #\Newline #\Tab #\Page #\Null #\Linefeed)
  "A list of characters that should be treated as whitespace. See, 
for example, [whitespacep][].")

(defun whitespacep (char)
  "Returns true if `char` is an element of [+whitespace-characters+][]
and nil otherwise."
  (not (null (find char +whitespace-characters+ :test #'char=))))

(defun string-trim-if (predicate string &key (start 0) (end (length string)))
  (let ((end (1- end)))
    (loop for ch across string 
       while (funcall predicate ch) do (incf start))
    (when (< start end)
      (loop for ch = (aref string end)
         while (funcall predicate ch) do (decf end)))
    (subseq string start (1+ end))))

(defun strip-whitespace (string &key (start 0) (end (length string)))
  (string-trim-if
   #'whitespacep string :start start :end end))

(defun lisp-version-string ()
  ;; shared with many other projects... sigh
  #+cmu       (substitute #\- #\/ 
			  (substitute #\_ #\Space 
				      (lisp-implementation-version)))
  #+scl       (lisp-implementation-version)
  #+sbcl      (lisp-implementation-version)
  #+ecl       (reduce (lambda (x str) (substitute #\_ str x))
		      '(#\Space #\: #\( #\)) 
		      :initial-value (lisp-implementation-version))
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4))
  #+openmcl   (format nil "~d.~d~@[-~d~]"
                      ccl::*openmcl-major-version* 
                      ccl::*openmcl-minor-version*
                      #+ppc64-target 64 
                      #-ppc64-target nil)
  #+lispworks (format nil "~A~@[~A~]"
                      (lisp-implementation-version)
                      (when (member :lispworks-64bit *features*) "-64bit"))
  #+allegro   (format nil
                      "~A~A~A~A"
                      excl::*common-lisp-version-number*
					; ANSI vs MoDeRn
		      ;; thanks to Robert Goldman and Charley Cox for
		      ;; an improvement to my hack
		      (if (eq excl:*current-case-mode* 
			      :case-sensitive-lower) "M" "A")
		      ;; Note if not using International ACL
		      ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
		      (excl:ics-target-case
			(:-ics "8")
			(:+ics ""))
                      (if (member :64bit *features*) "-64bit" ""))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+digitool   (subseq (lisp-implementation-version) 8))

(defun print-lift-message (message &rest args)
  (declare (dynamic-extent args))
  (apply #'format *lift-debug-output* message args)
  (force-output *lift-debug-output*))

(defun %start-test-case (name result)
  (when (and *test-print-test-case-names*
	     (eq (test-mode result) :multiple))
    (if (eq *test-print-test-case-names* :brief)
	(print-lift-message ".")
	(print-lift-message "~&  run: ~a" name))))

(defun %start-test-suite (name result)
  (when (and *test-print-testsuite-names*
	     (eq (test-mode result) :multiple))
    (if (eq *test-print-testsuite-names* :brief)
	(print-lift-message "*")
	(print-lift-message "~&Start: ~a" name))))

(defun safe-find-symbol (symbol package)
  (and (find-package package)
       (find-symbol (etypecase symbol
		      (string symbol)
		      (symbol (symbol-name symbol))) package)))

(defun symbol-apply (symbol package &rest args)
  (let* ((symbol (safe-find-symbol symbol package))
	 (function (and symbol (symbol-function symbol))))
    (when function
      (apply function args))))

(defun form-groups (list size)
  (let ((result nil)
	(count 0)
	(sub-result nil))
    (flet ((add-one ()
	     (push (nreverse sub-result) result)
	     (setf sub-result nil count 0)))
      (loop for a in list do
	   (when (= count size) (add-one))
	   (push a sub-result) 
	   (incf count))
      (when (= count size) (add-one))
      (values (nreverse result) (nreverse sub-result)))))

(defun test-function-name (test-function)
  (cond ((eq test-function #'eq)
	 "eq")
	((eq test-function #'eql)
	 "eql")
	((eq test-function #'equal)
	 "equal")
	((eq test-function #'equalp)
	 "equalp")
	((eq test-function #'=)
	 "=")
	(t
	 (princ-to-string test-function))))

(defun hostname ()
  (or *hostname*
      (setf *hostname* 
	    (multiple-value-bind (r _ code)
		(or (symbol-apply (symbol-name '#:run-shell-command)
				  :trivial-shell "hostname -s")
		    (symbol-apply (symbol-name '#:command-output)
				  :excl.osi "hostname -s"))
	      (declare (ignore _))
	      (if (/= code 0) "unknown" (first r))))))

(defun dotted-pair-p (putative-pair)
  "Returns true if and only if `putative-pair` is a dotted-list. I.e.,
if `putative-pair` is a cons cell with a non-nil cdr."
  (and (consp putative-pair)
       (cdr putative-pair)
       (not (consp (cdr putative-pair)))))

(defun make-printable (thing)
  (cond ((dotted-pair-p thing)
	 (cons (make-printable (car thing))
	       (make-printable (cdr thing))))
	((keywordp thing)
	 thing)
	((symbolp thing)
	 (encode-symbol thing))
	((listp thing)
	 (mapcar #'make-printable thing))
	((typep thing 'standard-object)
	 (format nil "~a" thing))
	(t
	 thing)))

(defun encode-symbol (symbol)
  (cons (symbol-name symbol) 
	(package-name (symbol-package symbol))))

(defun def (name &optional (definition *current-definition*))
  (when definition (cdr (assoc name definition))))

(defun (setf def) (value name)
  (set-definition name value))

(defun %build-ensure-comparison
    (form values guard-fn test test-specified-p report arguments
     ignore-multiple-values?)
  (setf test (remove-leading-quote test))
  (when (and (consp test)
             (eq (first test) 'function))
    (setf test (second test)))
  (let ((gblock (gensym "block-"))
	(ga (gensym "a-"))
	(gb (gensym "b-"))
	(gtest (gensym "test-")))
    `(block ,gblock
       (flet ((,gtest (,ga ,gb)
		(,@(cond (test-specified-p
			  (if (atom test) 
			      (list test)
			      `(funcall ,test)))
			 (t
			  `(funcall *lift-equality-test*)))
		   ,ga ,gb)))
	 (loop for value in (,(if ignore-multiple-values? 
				  'list 'multiple-value-list) ,form)
	    for other-value in (,(if ignore-multiple-values? 
				     'list 'multiple-value-list) ,values) do
	    (,guard-fn (,gtest value other-value)
		       (,(ecase guard-fn 
				(unless 'maybe-raise-not-same-condition)
				(when 'maybe-raise-ensure-same-condition))
			 value other-value
			 ,(if test-specified-p (list 'quote test) '*lift-equality-test*)
			 ,report ,@arguments)
		       (return-from ,gblock nil))))
       (values t))))
