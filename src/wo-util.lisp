;;;; wo-util.lisp

(uiop:define-package :wo-util/wo-util
  (:use :cl)
  (:export
   #:minimizing
   #:trim-spaces))
(in-package :wo-util/wo-util)


(defun minimizing (list f)
  "Returns from `list' the element that minimizes `f'.

Repeatedly call `f' on each list element of `list'.  The function
`f' should be a function taking one argument and returning a number which
can be compared with <."
  (loop 
     :with min-value = nil
     :with min-element = nil
     :finally (return min-element)
     :for e :in list 
     :for value = (funcall f e)
     :do
     (when (or (not min-value) (< value min-value))
       (setf min-value value)
       (setf min-element e))))

(defun trim-spaces (string)
  "Returns `string' with leading and trailing spaces stripped.
In this case a space is the literal #\Space character."
  (let ((end-position (position #\Space string :test-not #'eql :from-end t)))
    (if end-position
	(subseq string
		(position #\Space string :test-not #'eql)
		(1+ end-position))
	"")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
