(uiop:define-package :wo-util/approximate-strings
    (:use :cl :wo-util/priority-queue)
  (:import-from :alexandria #:ensure-list)
  (:export
   #:editing-distance))
(in-package :wo-util/approximate-strings)

;;;;;;;;;;;;;
(defun editing-distance (pattern texts &optional 
			 &key (anchored t) (cutoff nil))
  "Finds in the list of `texts' the text with the smallest editing distance to `pattern'.
The argument `text' is a string and the argument `texts' is either
a single string or a list of strings.  If `texts' is a single string it 
is treated as a list containing that string.

The function returns a pair '(matched-text . editing-distance)' for
which the matched-text is the element of `texts' with the lowest editing
distance.  The editing-distance is the editing distance between `pattern' and
matched-text.  
If `cutoff' is specified it will return `nil' if it can not find a matched-text
with editing distance < the cut off value.  
If `ancored' is nil it will not count as insertions the characters before the 
pattern starts and after the pattern ends in text."
  (let ((queue (make-priority-queue))
	(pattern (coerce pattern 'list))
	(texts (mapcar (lambda (txt) (coerce txt 'list))
		       (ensure-list texts))))

    (flet ((requeue (pat txt text priority)
	     (priority-queue-raise-priority
	      (list pat txt text)
	      priority
	      queue :test #'(lambda (a b) (every #'eq a b)))))

      (loop :for text :in texts :do 
	 (if anchored
	     (priority-queue-push (list pattern text text)
				  0 queue)
	     (loop :for txt :on text
		:do 
		(priority-queue-push (list pattern txt text) 0 queue))))
      (loop
	 :for (priority pat txt text) = (priority-queue-pop-with-priority queue)
	 :while (and (or pat txt)
		     (or (not cutoff) 
			 (< (- priority) cutoff)))
	 :finally (return (unless (and cutoff (>= (- priority) cutoff))  (cons text (- priority))))
	 :do
	 (if (and pat txt)
	     (progn
	       (requeue (rest pat) (rest txt) text
			(- priority (if (equal (car pat) (car txt)) 0 1)))
	       (requeue pat (rest txt) text (- priority 1))
	       (requeue  (rest pat) txt text (- priority 1)))
	     (requeue nil nil text 
		      (- priority (length pat)
			 (if anchored (length txt) 0))))))))
