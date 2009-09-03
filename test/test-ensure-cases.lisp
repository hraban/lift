(in-package #:lift-test)

;; wip

(deftestsuite test-ensure-cases (lift-test)
  ())

(addtest (test-ensure-cases)
  test-1
  (ensure-cases ((a b c))
      '((1 2 3)
	(2 3 4)
	(2 3 5)
	(5 6 11)
	(6 7 12)
	(8 -2 5)
	)
    (ensure-same (+ a b) c)))


(addtest (test-ensure-cases)
  test-1
  (ensure-cases ((a b c))
      '((1 2 3)
	(8 a 5)
	)
    (ensure-same (+ a b) c)))

(describe lift:*test-result*)

(lift:run-tests :suite 'lift-test)

#|
(setf p '((((8 -2 5)) "bad")
	  (((6 7 12)) "bad")
	  (((2 3 4)) "bad")))

(format 
 t 
 "~&~@<  ~@;~{~%  ~{~s ~3,8@t~a~}~^, ~}~:>" 
 p)

(format t "~@<;; ~@; ~{~a ~}~:@>"
	'("Ensure-same:" "12345678902469135780" "is" "not" "#<Function" "equal>" "to" "1234567890." "Ensure-same:" "12345678902469135780" "is" "not" "#<Function" "equal>" "to" "1234567890." "Ensure-same:" "12345678902469135780" "is" "not" "#<Function" "equal>" "to" "1234567890"))

(let ((*test-print-length* 7)
      (*test-print-level* 5))
  (describe *test-result*))

|#