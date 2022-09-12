;;;; xcb.asd

(asdf:defsystem #:xcb
  :description "Describe xcb here"
  :author "Bruno Dias"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:xcb-core #:xcb-util #:xcb-glx) 
  :components ((:file "package")
               (:file "xcb")))
