(uiop:define-package :wo-util/file
    (:use :cl :wo-util/queue))
(in-package :wo-util/file)

(defgeneric last-n-lines (n input)
  (:documentation "Returns as a list the last n lines of input.   
Input can be a stream of a pathname.")
  
  (:method ((n integer) (input stream))
    (let ((q (make-queue)))
      (loop :for i :from 1
	    :for l = (read-line input nil nil)
	    :while l
	    :do
	       (queue-push l q)
	       (when (> i n)
		 (queue-pop q)))
      (queue-to-list q)))

  (:method (n (input pathname))
    (with-open-file (s input)
      (last-n-lines n s))))
