;;;-*- Mode: Lisp; Package: ASDF-LIFT -*-

#| simple-header

$Author: gwking $

Copyright (c) 2001-2006 Gary Warren King (gwking@metabang.com) 

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions: 

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE. 

|#

(defpackage :asdf-lift (:use #:asdf #:cl))
(in-package :asdf-lift)

(defsystem lift
  :version "1.0"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "LIsp Framework for Testing"
  :long-description "LIFT is yet another SUnit variant."
  
  :components ((:module "dev" 
                        :components ((:static-file "notes.text")
                                     
                                     (:file "lift")
                                      
                        #+Ignore
                                     (:file "prototypes"
                                            :depends-on ("lift"))))
               
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  
  :in-order-to ((test-op (load-op lift-test)))
  :perform (test-op :after (op c)
                    (describe (funcall (intern (symbol-name '#:run-tests) :lift) 
                                       :suite (intern (symbol-name '#:lift-test)
                                                      :lift-test))))
  :depends-on (moptilities)) 

