(in-package #:wo-util)

(defun reverse-table (table &optional &key (test (hash-table-test table)))
  "Takes a hashtable of node --> list of targets and
returns the table for the graph with all edges reversed."
  (let ((result (make-hash-table :test test)))
    (maphash (lambda (k v) 
	       (loop :for new-source :in v
		  :do (push k (gethash new-source result (list)))))
	     table)
    result))


(defun make-count-table (list &optional &key (test #'eql))
  "Returns a table with keys all elements in `list' and values
the number of times a key appears.  The `test' is used to determine when two keys
are equal and should be one of the test accepted by make-hash-table."
  (let ((result (make-hash-table :test test)))
    (loop :for el :in list :do
       (incf (gethash el result 0)))
    result))


(defun report-histogram (table &optional top-n)
  "Writes to the stream `t' histogram of `table'.
The `table' is assumed to have numeric values and those are used
to sort the list of keys in `table'.  Example result is:

Key 1       : highest value
Key 2       : second highest value
...
Key n       : lowest value.

The value sare compared with #'>."
  (loop 
     :with entries =  (sort (alexandria:hash-table-alist table) #'> :key #'cdr)
     :repeat (or top-n (hash-table-count table))
     :for (key . count) :in entries
     :do
     (format t "~20A: ~D~%" key count)))

