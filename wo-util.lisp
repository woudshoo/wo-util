;;;; wo-util.lisp

(in-package #:wo-util)


#+nil (defun set-equal (set-a set-b &optional &key (test #'eq))
  "Returns t if two sets (lists) are equal under the test predicate `test'."
  (and (= (length set-a) (length set-b))
       (not (set-difference set-a set-b :test test))))


;; I have the feeling this already existsw
(defun remove-from-set (list-a list-b &optional &key (test #'eq))
  "Returns a list containing all elements from `list-a' not present
in `list-b'.   The optional argument `test' is the function used
to determine when two elements are equal."
  (remove-if (lambda (v) (member v list-b :test test)) list-a))

(defun length-is (n list)
  "Returns t if (length list) equals n.
Functionally it is equivalent with
 (equal n (length list))
However it is more efficient for small `n' and long lists."
  (if (eql n 0)
    (not list)
    (loop 
       :repeat n
       :for el :on list
       :finally (return (and el (not (cdr el)))))))


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


;;; should be replaced by alexandria:ensure-list
(defun listify (item)
  "Wraps in a list if it is not a list. 
Usefull for functions with either take a list of items
or a single item."
  (if (listp item) item (list item)))

(defun add-non-nil (list-a list-b)
  "Returns a list containing all elements of `list-a'
and all non nil elements of `list-b'.

The resulting list shares structure with list-a."
  (let ((result list-a))
    (loop :for element :in list-b :do
       (when element (push element result)))
    result))

(defun flatten-list (list)
  "should use alexandria ???"
  (let ((result))
    (labels ((flatten-internal (elements)
	     (loop :for x :in elements :do
		(if (listp x) (flatten-internal x)
		    (push x result)))))
      (flatten-internal list))
    (nreverse  result)))

