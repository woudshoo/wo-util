(in-package #:wo-util)

(deftype queue () '(cons))

(declaim (ftype (function () queue) make-queue))
(defun make-queue ()
  "Creates a FIFO queue"
  (let ((cell (cons nil nil)))
    (cons cell cell)))

(declaim (ftype (function (t queue) queue) queue-push))
(defun queue-push (element queue)
  "Adds `element' to the back of the FIFO `queue'.
This operation is destructive on `queue'.
The new queue is returned (which is the same place as the original queue)."
  (setf (cadr queue) element)
  (setf (cddr queue) (cons nil nil))
  (setf (cdr queue) (cddr queue))
  queue)

(declaim (ftype (function (queue) t) queue-pop))
(defun queue-pop (queue)
  "Removes and returns the element that is at the front of the `queue'.
The behaviour is undefined if the queue is empty."
  (pop (car  queue)))

(declaim (ftype (function (queue) boolean) queue-empty-p))
(defun queue-empty-p (queue)
  "Returns t if the queue is empty."
  (eq (car queue) (cdr queue)))

(declaim (ftype (function (queue) fixnum) queue-length))
(defun queue-length (queue)
  "Returns number of items in the queue.  
This function is O(n) in the length of the queue."
  (- (length (car queue)) 1))

(declaim (ftype (function (queue) queue) queue-copy))
(defun queue-copy (queue)
  "Make a copy of the queue."
  (let ((result (make-queue)))
    (loop :for e :on (car queue)
       :until (eq e (cdr queue))
       :do
	 (queue-push (car e) result))
    result))

(declaim (ftype (function (queue) list) queue-to-list))
(defun queue-to-list (queue)
  "Returns the content of the queue as list"
  (loop :repeat (queue-length queue)
	:for e :in (car queue)
	:collect e))

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

