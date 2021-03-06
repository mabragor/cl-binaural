;;;; cl-binaural.asd

(asdf:defsystem #:cl-binaural
  :serial t
  :description "Utilities to generate binaural sound from mono"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:mixalot #:iterate)
  :components ((:file "mixalot-dummy-patch")
	       (:file "package")
               (:file "cl-binaural")))

