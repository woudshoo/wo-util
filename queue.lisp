(in-package #:wo-util)

(defun make-queue ()
  "Creates a FIFO queue"
  (let ((cell (cons nil nil)))
    (cons cell cell)))

(defun queue-push (element queue)
  "Adds `element' to the back of the FIFO `queue'."
  (setf (cadr queue) element)
  (setf (cddr queue) (cons nil nil))
  (setf (cdr queue) (cddr queue))
  queue)

(defun queue-pop (queue)
  "Removes and returns the element that is at the front of the `queue'.
The behaviour is undefined if the queue is empty."
  (pop (car  queue)))

(defun queue-empty-p (queue)
  "Returns t if the queue is empty."
  (eq (car queue) (cdr queue)))


(defun queue-copy (queue)
  "Make a copy of the queue."
  (let ((result (make-queue)))
    (loop :for e :on (car queue)
       :until (eq e (cdr queue))
       :do
	 (queue-push (car e) result))
    result))

(defmacro do-queue ((queue-element queue) &body body)
  "Executes body repeatedly for each element in the `queue'.
The queue element will be bound to the var queue-element.

It is allowed to modify the queue in the body.

NOTE:  The macro need to be improved because queue is repeatedly
evaluated.
"
  `(loop :until (queue-empty-p ,queue)
      :for ,queue-element = (queue-pop ,queue)
      :do
      (progn
	,@body)))

