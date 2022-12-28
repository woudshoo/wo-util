;;;; package.lisp

(defpackage #:wo-util
  (:use #:cl)
  (:export
   ;;; queue
   #:make-queue
   #:queue-copy
   #:queue-push
   #:queue-pop
   #:queue-empty-p
   #:queue-length
   #:do-queue
   ;;; priority queue
   #:make-priority-queue
   #:priority-queue-push
   #:priority-queue-pop-with-priority
   #:priority-queue-pop
   #:priority-queue-top
   #:priority-queue-empty-p
   ;;; table functions
   #:reverse-table
   ;; #:table-keys  should use alexandria
   #:make-count-table
   ;; #:table-to-alist should use alexandria
   #:report-histogram
   ;;; wo-util
   ;; #:set-equal should use alexandria
   #:remove-from-set
   #:length-is
   #:minimizing
   #:trim-spaces
   #:listify  ;; should be replaced
   #:add-non-nil
   #:flatten-list
   ;;; approximate-strings
   #:editing-distance
   ;;; png functions
   #:png-size
   ;; #:png-list-chunks
   #:add-value-to-map
   #:priority-queue-top-priority))
