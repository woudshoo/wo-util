(in-package #:wo-util)

(defun priority-queue-trickle-up (index queue)
  "takes the element at `index' of `queue' and moves it up 
the tree while the parent is smaller.
Returns the index where the element stops trickling up."
  (loop 
     :for parent = (ash (1- index) -1)
     :while (and (>= parent 0)
		 (> (car (aref queue index)) 
		    (car (aref queue parent))))
     :finally (return index)
     :do
     (rotatef (aref queue index) (aref queue parent))
     (setf index parent)))

(defun priority-queue-trickle-down (index queue)
  "takes the element at `index' of `queue' and moves it down
the tree while the lement is smaller than one of the children.
Returns the index of where the element ended."
  (loop 
     :with length = (length queue)
     :with value = (car (aref queue index))
     :for candidate-value = value
     :for candidate = nil
     :for child = (ash index 1)
     :do 
     (incf child)
     (when (and (> length child) (> (car (aref queue child)) candidate-value))
       (setf candidate child)
       (setf candidate-value (car (aref queue child))))
     (incf child)
     (when (and (> length child) (> (car (aref queue child)) candidate-value))
       (setf candidate child))
     (if candidate
	 (progn
	   (rotatef (aref queue index) (aref queue candidate))
	   (setf index candidate))
	 (return index))))

(defun make-priority-queue ()
  "Creates a priority queue with comparison #'>."
  (make-array 0 :adjustable t :fill-pointer t))

(defun priority-queue-push (item priority queue)
  "Adds `item' to the queue given `priority' in the `queue'.
The priority argument should be compareable with #'> to determine the order.
"
  (priority-queue-trickle-down
   (priority-queue-trickle-up 
    (vector-push-extend (cons priority item) queue)
    queue)
   queue))

(defun priority-queue-raise-priority (item priority queue &optional &key (test #'eql))
  "If the `item' is not in the `queue' already it will add it to the queue.
Otherwise it will update the item to have a new `priority', but only 
if the prioirty is higher than the original priority

Not that this is not particulary efficient."
  (let ((index (position item queue :test test :key #'cdr)))
    (if index
	(when (> priority (car (aref queue index)))
	  (setf (car (aref queue index)) priority)
	  (priority-queue-trickle-down
	   (priority-queue-trickle-up index queue)
	   queue))
	(priority-queue-push item priority queue))))

(defun priority-queue-pop-with-priority (queue)
  "Returns the pair (priority . element)
with the highest priority from the `queue'. The element
will be removed from the queue as well. 

Calling this on an empty queue is undefined."
  (rotatef (aref queue 0)
	   (aref queue (1- (length queue))))
  (prog1
      (vector-pop queue)
    (priority-queue-trickle-down 0 queue)))

(defun priority-queue-pop (queue)
  "Returns the element with the highest priority from the `queue'. The element
will be removed from the queue as well. 

Calling this on an empty queue is undefined."
  (cdr (priority-queue-pop-with-priority queue)))

(defun priority-queue-top (queue)
  "Returns the element with the highest prioirty from the `queue'.  
In contrast with the pop operation it will not remove the element from the queue.

Calling this on an empty queue is undefined."
  (cdr (aref queue 0)))

(defun priority-queue-empty-p (queue)
  "Returns t if the `queue' contains no elements.  Returns nil otherwise."
  (zerop (length queue)))

