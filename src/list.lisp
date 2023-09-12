(uiop:define-package :wo-util/list
    (:use :cl)
  (:export
   #:remove-from-set
   #:length-is
   #:add-non-nil))
(in-package :wo-util/list)

;; I have the feeling this already existsw
(defun remove-from-set (list-a list-b &optional &key (test #'eq))
  "Returns a list containing all elements from `list-a' not present
in `list-b'.   The optional argument `test' is the function used
to determine when two elements are equal."
  (remove-if (lambda (v) (member v list-b :test test)) list-a))


(defun add-non-nil (list-a list-b)
  "Returns a list containing all elements of `list-a'
and all non nil elements of `list-b'.

The resulting list shares structure with list-a."
  (let ((result list-a))
    (loop :for element :in list-b :do
       (when element (push element result)))
    result))

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


#+nil (defun set-equal (set-a set-b &optional &key (test #'eq))
  "Returns t if two sets (lists) are equal under the test predicate `test'."
  (and (= (length set-a) (length set-b))
       (not (set-difference set-a set-b :test test))))

