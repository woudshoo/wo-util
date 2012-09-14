;;;; wo-util.asd

(asdf:defsystem #:wo-util
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "wo-util")
	       (:file "queue")
	       (:file "priority-queue")
	       (:file "table-functions")
	       (:file "approximate-strings")
	       (:file "png-size")))

