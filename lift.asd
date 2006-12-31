(defpackage #:asdf-lift (:use #:asdf #:cl))
(in-package #:asdf-lift)

(defsystem lift
  :version "1.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "LIsp Framework for Testing"
  :long-description "LIFT is yet another SUnit variant."
  
  :components ((:module "dev" 
                        :components ((:static-file "notes.text")
                                     
                                     (:file "lift")
				     (:file "random-testing" 
					    :depends-on ("lift"))
				     (:file "port" 
					    :depends-on ("lift"))
                                      
				     #+Ignore
                                     (:file "prototypes"
                                            :depends-on ("lift"))))
               
               (:module 
		"website"
		:components ((:module "source"
				      :components 
				      ((:static-file "index.lml"))))))
  
  :in-order-to ((test-op (load-op lift-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:lift-test)))
  :depends-on (#+tam moptilities)) 

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'lift))))
  (values nil))
