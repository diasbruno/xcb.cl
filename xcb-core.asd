;;;; xcb-core.asd

(asdf:defsystem #:xcb-core
  :description "Describe xcb-core here"
  :author "Bruno Dias"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi)
  :components ((:file "xcb-core")))

(defpackage :xcb-core
  (:use :cl :cffi)
  (:export
   #:window
   #:void-cookie
   #:colormap
   #:visualid
   #:screen
   #:connection-error
   #:conn-ok
   #:window-class
   #:connect
   #:disconnect
   #:connection-has-error
   #:get-setup
   #:generate-id
   #:create-window
   #:map-window
   #:unmap-window
   #:flush
   #:pixmap
   #:cursor
   #:font
   #:gcontext
   #:atom
   #:visual-class
   #:connection
   #:create-colormap
   #:screen->root
   #:screen->root-visualid
   #:screen->root-depth
   #:screen->white-pixel
   #:generic-error))
