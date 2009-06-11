(in-package #:lift-test)

#+(or)
(let ((lift:*test-print-test-case-names* t))
  (run-tests :suite 'report-pathname-abstract))

#+(or)
;; this is failing ... ugh
(deftestsuite report-pathname-abstract ()
  ((result (make-test-result nil :multiple))))

(deftestsuite report-pathname-abstract ()
  (result)
  (:setup
   (setf result (make-test-result nil :multiple))))

(addtest (report-pathname-abstract)
  initial-properties-are-null
  (ensure-null (lift::test-result-properties result)))

(deftestsuite report-pathname-describe (report-pathname-abstract)
  ()
  (:setup
   (setf (lift::test-result-property result :format) :describe)))

(addtest (report-pathname-describe)
  default-pathname-based-on-current-directory
  (ensure-same 
   (namestring (merge-pathnames "report.describe"))
   (namestring (lift::report-pathname :describe result))
   :test 'string=))

(addtest (report-pathname-describe)
  full-pathname-non-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/")
  (ensure-same 
   "/tmp/foo/report.describe"
   (namestring (lift::report-pathname :describe result))
   :test 'string=))

(addtest (report-pathname-describe)
  full-pathname-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/"
	(lift::test-result-property result :unique-name)
	t)
  (let ((dest (namestring (lift::report-pathname :describe result))))
    (ensure-same '(:absolute "tmp" "foo")
		 (pathname-directory dest) :test 'equalp) 
    (ensure-same (pathname-type dest) "describe" :test 'string=)
    (ensure-same (subseq (pathname-name dest) 0 7)
		 "report-" :test 'string=)))

(addtest (report-pathname-describe)
  full-pathname-and-name-non-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/"
	(lift::test-result-property result :name)
	"report.html"
	(lift::test-result-property result :unique-name)
	nil)
  (ensure-same 
   (namestring (lift::report-pathname :describe result))
   "/tmp/foo/report.html"
   :test 'string=))

(addtest (report-pathname-describe)
  full-pathname-and-name-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/"
	(lift::test-result-property result :name)
	"report.html"
	(lift::test-result-property result :unique-name)
	t)
  (let ((dest (namestring (lift::report-pathname :describe result))))
    (ensure-same '(:absolute "tmp" "foo")
		 (pathname-directory dest) :test 'equalp) 
    (ensure-same (pathname-type dest) "html" :test 'string=)
    (ensure-same (subseq (pathname-name dest) 0 7)
		 "report-" :test 'string=)))

(addtest (report-pathname-describe)
  full-pathname-provides-name-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/test.txt"
	(lift::test-result-property result :name)
	"report.html"
	(lift::test-result-property result :unique-name)
	t)
  (let ((dest (namestring (lift::report-pathname :describe result))))
    (ensure-same '(:absolute "tmp" "foo")
		 (pathname-directory dest) :test 'equalp) 
    (ensure-same (pathname-type dest) "txt" :test 'string=)
    (ensure-same (subseq (pathname-name dest) 0 5)
		 "test-" :test 'string=)))

(addtest (report-pathname-describe)
  full-pathname-provides-name-non-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/test.txt"
	(lift::test-result-property result :name)
	"report.html")
  (let ((dest (namestring (lift::report-pathname :describe result))))
    (ensure-same '(:absolute "tmp" "foo")
		 (pathname-directory dest) :test 'equalp) 
    (ensure-same (pathname-type dest) "txt" :test 'string=)
    (ensure-same (subseq (pathname-name dest) 0 4)
		 "test" :test 'string=)))

(addtest (report-pathname-describe)
  full-pathname-is-stream
  (setf (lift::test-result-property result :full-pathname)
	*standard-output*)
  (let ((dest (lift::report-pathname :describe result)))
    (ensure-same dest *standard-output*)))

(addtest (report-pathname-describe)
  full-pathname-is-stream-unique
  (setf (lift::test-result-property result :full-pathname)
	*standard-output*
	(lift::test-result-property result :unique-name)
	t)
  (let ((dest (lift::report-pathname :describe result)))
    (ensure-same dest *standard-output*)))

(addtest (report-pathname-describe)
  relative-to-and-name-non-unique
  (setf (lift::test-result-property result :relative-to)
	:lift
	(lift::test-result-property result :name)
	"xxx.yyy"
	(lift::test-result-property result :unique-name)
	nil)
  (let ((destination (lift::report-pathname :describe result)))
    ;(print (list :x destination))
    (ensure-same (pathname-name destination) "xxx" :test 'string=)
    (ensure-same (pathname-type destination) "yyy" :test 'string=)
    (ensure-same 
     (pathname-directory destination)
     (pathname-directory (asdf:system-source-file :lift))
     :test 'equalp)))

(addtest (report-pathname-describe)
  relative-to-and-name-unique
  (setf (lift::test-result-property result :relative-to)
	:lift
	(lift::test-result-property result :name)
	"xxx.yyy"
	(lift::test-result-property result :unique-name)
	t)
  (let ((destination (lift::report-pathname :describe result)))
					;(print (list :x destination))
    (ensure-same (subseq (pathname-name destination) 0 4)
		 "xxx-" :test 'string=)
    (ensure-same (pathname-type destination) "yyy" :test 'string=)
    (ensure-same 
     (pathname-directory destination)
     (pathname-directory (asdf:system-source-file :lift))
     :test 'equalp)))

;;;;;

(deftestsuite report-pathname-html (report-pathname-abstract)
  ()
  (:setup
   (setf (lift::test-result-property result :format) :html)))

(addtest (report-pathname-html)
  full-pathname-and-name-ignored-non-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/"
	(lift::test-result-property result :name)
	"reports"
	(lift::test-result-property result :unique-name)
	nil)
  (ensure-same 
   (namestring (lift::report-pathname :html result))
   "/tmp/foo/index.html"
   :test 'string=))

(addtest (report-pathname-html)
  full-pathname-and-name-ignored-unique
  (setf (lift::test-result-property result :full-pathname)
	"/tmp/foo/"
	(lift::test-result-property result :name)
	"xxx"
	(lift::test-result-property result :unique-name)
	t)
  (let ((destination (lift::report-pathname :html result)))
    (print (list :x destination))
    (ensure-same (pathname-name destination) "index" :test 'string=)
    (ensure-same (pathname-type destination) "html" :test 'string=)
    (ensure-same (subseq (pathname-directory destination) 0 2)
		 '(:absolute "tmp") :test 'equalp)
    (ensure-same (length (pathname-directory destination)) 3)
    (ensure-same (subseq (third (pathname-directory destination)) 0 4)
		 "foo-") :test 'string=))

(addtest (report-pathname-html)
  relative-to-and-name-non-unique
  (let ((name "xxx.yyy"))
    (setf (lift::test-result-property result :relative-to)
	  :lift
	  (lift::test-result-property result :name)
	  name
	  (lift::test-result-property result :unique-name)
	  nil)
    (let ((destination (lift::report-pathname :html result)))
      (ensure-same (pathname-name destination) "index" :test 'string=)
      (ensure-same (pathname-type destination) "html" :test 'string=)
      (ensure-same 
       (namestring destination)
       (namestring 
	(merge-pathnames 
	 "index.html"
	 (merge-pathnames (make-pathname :directory `(:relative ,name))
			  (asdf:system-source-file :lift))))
       :test 'string=))))

(addtest (report-pathname-html)
  relative-to-and-name-unique
  (setf (lift::test-result-property result :relative-to)
	:lift
	(lift::test-result-property result :name)
	"xxx.yyy"
	(lift::test-result-property result :unique-name)
	t)
  (let ((destination (lift::report-pathname :html result)))
    (print (list :x destination))
    (ensure-same (pathname-name destination) "index" :test 'string=)
    (ensure-same (pathname-type destination) "html" :test 'string=)
    (ensure-same 
     (butlast (pathname-directory (namestring destination)))
     (pathname-directory (asdf:system-source-file :lift))
     :test 'equalp)
    (ensure-same (subseq (first (last (pathname-directory destination))) 0 4)
		 "xxx-") :test 'string=))

