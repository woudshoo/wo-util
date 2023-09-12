;;;; wo-util.asd

(asdf:defsystem #:wo-util
  :serial t
  :depends-on (#:wo-util/package)
  :class :package-inferred-system
  :pathname "src/")



#|
:components  ((:file "package")
	      (:file "wo-util")
	      (:file "queue")
	      (:file "priority-queue")
	      (:file "table-functions")
	      (:file "approximate-strings")
	      (:file "png-size"))

|#
