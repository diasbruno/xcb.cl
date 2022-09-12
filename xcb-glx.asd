;;;; xcb-glx.asd

(asdf:defsystem #:xcb-glx
  :description "Describe xcb-glx here"
  :author "Bruno Dias"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:xcb-core)
  :components ((:file "xcb-glx")))

(defpackage :xcb-glx
  (:use #:cl #:cffi)
  (:export
   #:create-new-context
   #:query-version
   #:query-version-reply
   #:query_version_reply
   #:query-version-reply-data
   #:create-window
   #:pixmap
   #:pbuffer
   #:context
   #:window
   #:fbconfig
   #:drawable
   #:context-tag
   #:make-current-cookie
   #:make-context-current-cookie
   #:query-version-cookie
   #:get-fb-configs-cookie
   #:query-context-cookie
   #:pbcet
   #:pbcdt
   #:gc
   #:rm
   #:make-context-current-reply-data
   #:get-fb-configs-reply
   #:get-fb-configs
   #:get-fb-configs-property-list
   #:get-fb-configs-reply-data
   #:fb-config-render-type
   #:fb-rgba-bit
   #:fb-color-index-bit
   #:fb-pbuffer-clobber-mask
   #:fb-front-left-buffer-bit
   #:fb-front-right-buffer-bit
   #:fb-back-left-buffer-bit
   #:fb-back-right-buffer-bit
   #:fb-aux-buffers-bit
   #:fb-depth-buffer-bit
   #:fb-stencil-buffer-bit
   #:fb-accum-buffer-bit
   #:fb-vendor
   #:fb-version
   #:fb-extensions
   #:fb-config-drawable-type
   #:fb-window-bit
   #:fb-pixmap-bit
   #:fb-pbuffer-bit
   #:fb-config-attrs
   #:fb-use-gl
   #:fb-buffer-size
   #:fb-level
   #:fb-rgba
   #:fb-doublebuffer
   #:fb-stereo
   #:fb-aux-buffers
   #:fb-red-size
   #:fb-green-size
   #:fb-blue-size
   #:fb-alpha-size
   #:fb-depth-size
   #:fb-stencil-size
   #:fb-accum-red-size
   #:fb-accum-green-size
   #:fb-accum-blue-size
   #:fb-accum-alpha-size
   #:fb-config-caveat
   #:fb-x-visual-type
   #:fb-transparent-type
   #:fb-transparent-index-value
   #:fb-transparent-red-value
   #:fb-transparent-green-value
   #:fb-transparent-blue-value
   #:fb-transparent-alpha-value
   #:fb-dont-care
   #:fb-none
   #:fb-slow-config
   #:fb-true-color
   #:fb-direct-color
   #:fb-pseudo-color
   #:fb-static-color
   #:fb-gray-scale
   #:fb-static-gray
   #:fb-transparent-rgb
   #:fb-transparent-index
   #:fb-visual-id
   #:fb-screen
   #:fb-non-conformant-config
   #:fb-drawable-type
   #:fb-render-type
   #:fb-x-renderable
   #:fb-fbconfig-id
   #:fb-rgba-type
   #:fb-color-index-type
   #:fb-max-pbuffer-width
   #:fb-max-pbuffer-height
   #:fb-max-pbuffer-pixels
   #:fb-preserved-contents
   #:fb-largest-pbuffer
   #:fb-width
   #:fb-height
   #:fb-event-mask
   #:fb-damaged
   #:fb-saved
   #:fb-window
   #:fb-pbuffer
   #:fb-pbuffer-height
   #:fb-pbuffer-width
   #:make-context-current
   #:make-context-current-reply))
