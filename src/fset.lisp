(uiop:define-package :wo-util/fset
    (:use :cl)
  (:import-from :fset #:with)
  (:export
   #:add-value-to-map))
(in-package :wo-util/fset)


(defun add-value-to-map (map key value)
  "Add a value to list of values of map[key].
Map is an fset style map"
  (let ((new-value (cons value (fset:lookup map key))))
    (with map key new-value)))
