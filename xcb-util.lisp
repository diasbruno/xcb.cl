;;;; xcb-util.lisp

(in-package #:xcb-util)

(cffi:define-foreign-library xcb-util
  (:unix "/nix/store/gv9qi2h3cn6bl44gljdmhdwip2h5b20g-xcb-util-0.4.0/lib/libxcb-util.so"))

(cffi:load-foreign-library 'xcb-util)

(cffi:defcfun ("xcb_aux_get_screen" xcb-aux-get-screen)
    (:pointer (:struct xcb-core:screen))
  "Get a indexed SCREEN of a connection."
  (connection :pointer)
  (screen :int))
