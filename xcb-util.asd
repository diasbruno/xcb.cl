;;;; xcb-util.asd

(asdf:defsystem #:xcb-util
  :description "Describe xcb-util here"
  :author "Bruno Dias"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:xcb-core)
  :components ((:file "xcb-util")))

(defpackage :xcb-util
  (:use :cl :cffi)
  (:export
   #:xcb-aux-get-screen))
