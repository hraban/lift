(defpackage #:lift-system (:use #:common-lisp #:asdf))
(in-package #:lift-system)

(defsystem lift
  :version "1.5.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "LIsp Framework for Testing"
  :long-description "LIFT is an SUnit variant and much much more."  
  :components ((:module 
		"timeout"
		:pathname "dev/"
		:components 
		((:file "with-timeout")))
	       (:module
		"dev" 
		:depends-on ("timeout")
		:components 
		((:static-file "notes.text")
             
		 (:file "packages")
		 (:file "utilities" 
			:depends-on ("packages"))
		 (:file "lift"
			:depends-on ("packages" "measuring" "port" "utilities"))
		 (:file "copy-file"
			:depends-on ("packages"))
		 (:file "random-testing" 
			:depends-on ("packages" "lift"))
		 (:file "port" 
			:depends-on ("packages"))
		 (:file "measuring" 
			:depends-on ("packages" "port"))
		 (:file "config" 
			:depends-on ("port" "lift"))
		 (:file "reports" 
			:depends-on ("port" "lift"))
		 (:file "introspection" 
			:depends-on ("lift"))
		  #+Ignore
		 (:file "prototypes"
			:depends-on ("lift"))))
               
	       #+(or)
               (:module 
		"website"
		:components ((:module "source"
				      :components 
				      ((:static-file "index.md"))))))
  
  :in-order-to ((test-op (load-op lift-test)))
  :depends-on ()
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic)))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'lift))))
  (values nil))


(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

#+asdf-system-connections
(asdf:defsystem-connection lift-report-locations
  :requires (:lift :asdf-binary-locations)
  :components ((:module "dev"
			:components ((:file "report-locations")))))
