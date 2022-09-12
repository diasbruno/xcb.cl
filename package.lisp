;;;; package.lisp

(defpackage #:xcb
  (:use #:cl)
  (:export
   #:fb-find-by-attr
   #:has-invalid-attrs
   #:find-fb-config-by-attributes
   #:find-fb-attr-array
   #:default-connection
   #:glx-create-new-context
   #:create-colormap
   #:create-window))
