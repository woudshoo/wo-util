(uiop:define-package :wo-util/duplicate-code
    (:use :cl)
  (:import-from :alexandria #:flatten #:ensure-list)
  (:export
   #:listify
   #:flatten-list))
(in-package :wo-util/duplicate-code)



(defun listify (item)
  (ensure-list item))
;;; Old code
#+nil (defun listify (item)
  "Wraps in a list if it is not a list. 
Usefull for functions with either take a list of items
or a single item."
  (if (listp item) item (list item)))

(defun flatten-list (list)
  (flatten list))
;;; Old Code
#+nil (defun flatten-list (list)
  "should use alexandria ???"
  (let ((result))
    (labels ((flatten-internal (elements)
	     (loop :for x :in elements :do
		(if (listp x) (flatten-internal x)
		    (push x result)))))
      (flatten-internal list))
    (nreverse  result)))

