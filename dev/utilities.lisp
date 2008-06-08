(in-package #:lift)

;; borrowed from asdf
(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

;; FIXME -- abstract and merge with unique-directory
(defun unique-filename (pathname)
  (let ((date-part (date-stamp)))
    (loop repeat 100
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
    (error "Unable to find unique pathname for ~a" pathname)))
	    
;; FIXME -- abstract and merge with unique-filename
(defun unique-directory (pathname)
  (when (or (pathname-name pathname) (pathname-type pathname))
    (setf pathname (make-pathname 
		    :name nil
		    :type nil
		    :directory `(,@(pathname-directory pathname)
				   ,(format nil "~a~@[.~a~]"
					    (pathname-name pathname) 
					    (pathname-type pathname)))
		    :defaults (pathname-sans-name+type pathname))))
  (or (and (not (probe-file pathname)) pathname)
      (let ((date-part (date-stamp)))
	(loop repeat 100
	   for index from 1
	   for name = 
	   (merge-pathnames 
	    (make-pathname
	     :name nil
	     :type nil
	     :directory `(:relative 
			  ,(format nil "~@[~a-~]~a-~d" 
				   (and (stringp (pathname-name pathname))
					(pathname-name pathname))
				   date-part index)))
	    (pathname-sans-name+type pathname)) do
	   (unless (probe-file name)
	     (return name))))
      (error "Unable to find unique pathname for ~a" pathname)))

(defun date-stamp (&key (datetime (get-universal-time)) (include-time? nil))
  (multiple-value-bind
	(second minute hour day month year day-of-the-week)
      (decode-universal-time datetime)
    (declare (ignore day-of-the-week))
    (let ((date-part (format nil "~d-~2,'0d-~2,'0d" year month day))
	  (time-part (and include-time? 
			  (list (format nil "-~2,'0d-~2,'0d-~2,'0d"
					hour minute second)))))
      (apply 'concatenate 'string date-part time-part))))

#+(or)
(date-stamp :include-time? t)	

