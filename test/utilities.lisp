(in-package #:lift-test)

(defun set-equal (list1 list2 &rest args
		  &key test key)
  "Returns t if list1 and list2 are equal (as sets). If list1 and list2 are not equal returns (as multiple values) nil and two lists. The first list contains the elements in list1 and not in list2 and the second list contains elements in list2 and not in list1."
  (declare (ignore test key))
  (let ((in1-not2 (apply #'set-difference list1 list2 args))
        (in2-not1 (apply #'set-difference list2 list1 args)))
    (if (or in1-not2 in2-not1)
      (values nil in1-not2 in2-not1)
      (values t nil nil))))
