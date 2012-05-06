(in-package #:wo-util)


(defun read-u4-be (stream)
  (let ((result 0))
    (setf (ldb (byte 8 24) result) (read-byte stream))
    (setf (ldb (byte 8 16) result) (read-byte stream))
    (setf (ldb (byte 8 8) result) (read-byte stream))
    (setf (ldb (byte 8 0) result) (read-byte stream))
    result))

(defun test-read (stream)
  (loop :repeat 4 :collect (read-byte stream)))


(defun read-string (stream length)
  (coerce 
   (loop :repeat length
      :collect (code-char (read-byte stream)))
   'string))

(defun png-size (file-name)
  (with-open-file (s file-name :direction :input :element-type '(unsigned-byte 8))
    (file-position s 16)
    (cons (read-u4-be s) (read-u4-be s))))


(defun png-list-chunks (file-name)
  (with-open-file (s file-name :direction :input :element-type '(unsigned-byte 8))
    (file-position s 8)
    (loop 
       :for length = (read-u4-be s)
       :for name = (read-string s 4)
       :collect  (cons name length)
       :until (string-equal "IEND" name)
       :do 
       (format t "~A: ~A~%" name length)
       (file-position s (+ (file-position s) length 4)))))