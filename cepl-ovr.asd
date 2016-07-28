;;;; cepl-ovr.asd

(asdf:defsystem #:cepl-ovr
  :description "Describe cepl-ovr here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl.glop
	       #:3b-ovr-sample
               #:temporal-functions
               #:swank
               #:livesupport
               #:skitter
               #:cepl.devil)
  :serial t
  :components ((:file "package")
               (:file "cepl-ovr")))

