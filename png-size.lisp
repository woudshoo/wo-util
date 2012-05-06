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
  "Returns the size in pixels of the png file `file-name'.
The size is returned as a cons cell (width . height).
Note that no checking is done if the file is a valid png image
it just assumes it is and reads the bytes at the offsets specified
in the png specificaiton."
  (with-open-file (s file-name :direction :input :element-type '(unsigned-byte 8))
    (file-position s 16)
    (cons (read-u4-be s) (read-u4-be s))))


(defun png-list-chunks (file-name)
  "Prints a list of chunks in the png file to standard out (the t stream).
This is just a small experimental function and no checking error recovery etc
is even attempted."
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