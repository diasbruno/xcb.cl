;; https://cffi.common-lisp.dev/manual/cffi-manual.pdf
;; https://github.com/artem-yazkov/touch/blob/master/gstack/xcb/xcb-glx/ex1/test.c
;; https://xcb.freedesktop.org/manual/index.html
;; https://xcb.freedesktop.org/tutorial/basicwindowsanddrawing/

(dolist (p (list #P"/usr/local/src/cl-opengl"
		 #P"/usr/local/src/xcb.cl"))
  (push p ql:*local-project-directories*))

(ql:quickload '(#:cffi #:xcb #:xcb-core #:xcb-util #:xcb-glx :cl-opengl))

(defstruct application
  connection
  window
  colormap
  screennum
  screen
  glxwindow
  glxcontext
  glxcontextag)

(defparameter app nil)
(let ((cc (xcb:default-connection)))
  (setf app
	(make-application :connection cc
			  :window (xcb-core:generate-id cc)
			  :colormap (xcb-core:generate-id cc)
			  :screennum 0
			  :screen (xcb-util:xcb-aux-get-screen cc 0)
			  :glxwindow (xcb-core:generate-id cc)
			  :glxcontext (xcb-core:generate-id cc)
			  :glxcontextag 0)))

(defparameter fbconfig nil)
(defparameter xglxvisualid nil)

(with-slots (connection window glxwindow
	     screen screennum glxcontext colormap) app
  (setf fbconfig (xcb:find-fb-config-by-attributes connection
						   screennum
						   `((,xcb-glx::fb-doublebuffer . 1)
						     (,xcb-glx:fb-stencil-size . 8)
						     (,xcb-glx:fb-red-size . 8)
						     (,xcb-glx:fb-green-size . 8)
						     (,xcb-glx:fb-blue-size . 8)
						     (,xcb-glx:fb-alpha-size . 8)
						     (,xcb-glx:fb-depth-size . 24)
						     (,xcb-glx:fb-buffer-size . 32)
						     (,xcb-glx:fb-render-type . ,xcb-glx:fb-rgba-bit)
						     (,xcb-glx:fb-drawable-type . ,(logior xcb-glx:fb-window-bit
											   xcb-glx:fb-pixmap-bit
											   xcb-glx:fb-pbuffer-bit))
						     (,xcb-glx:fb-x-renderable . 1))))
  (setf xglxvisualid (xcb:find-fb-attr-array fbconfig xcb-glx::fb-visual-id))
  (format t "choosing fbconfig id 0x~x with visual id 0x~x ~a"
	  (xcb:find-fb-attr-array fbconfig xcb-glx::fb-fbconfig-id)
	  xglxvisualid
	  (terpri))
  (xcb:create-colormap connection
		       xcb-core::colormap-alloc-none
		       colormap
		       screen
		       xglxvisualid)
  (xcb:create-window connection
		     screen
		     window
		     0 0 500 400 0
		     xcb-core::window-class-input-output
		     xglxvisualid
		     (logior xcb-core::cw-back-pixel xcb-core::cw-colormap)
		     (list (xcb-core:screen->white-pixel screen) colormap))
  (xcb-core:map-window connection window)
  (xcb:glx-create-new-context connection
			      glxcontext
			      fbconfig
			      screennum)
  (xcb-glx:create-window connection
			 screennum
			 (xcb:find-fb-attr-array fbconfig
						 xcb-glx::fb-fbconfig-id)
			 window
			 glxwindow
			 0
			 (cffi:null-pointer))
  (let ((p0 (cffi:foreign-alloc :pointer)))
    (cffi:with-foreign-object (p1 :pointer)
      (setf (cffi:mem-aref p1 :pointer) p0)
      (let* ((mcc (xcb-glx:make-context-current connection
						0
						glxwindow
						glxwindow
						glxcontext))
	     (r (xcb-glx:make-context-current-reply
		 connection
		 mcc
		 p1)))
	(print mcc)
	(cffi:with-foreign-slots ((xcb-core::response-type
				   xcb-core::sequence)
				  p0
				  (:struct xcb-core:generic-error))
	  (format t "check error ~a ~a"
		  xcb-core::response-type
		  xcb-core::sequence))
	(cffi:with-foreign-slots ((xcb-glx::context-tag)
				  r
				  (:struct xcb-glx:make-context-current-reply-data))
	  (setf xglxctxtag xcb-glx::context-tag))
	(sleep 5))
      (cffi:foreign-free p0)))
  (xcb-core:flush connection)
  (xcb-core:disconnect connection))
;; trash

(defun get-glx-version (cc)
  (let* ((qv (xcb-glx:query-version cc 0 0))
	 (r (xcb-glx:query-version-reply cc qv (cffi:null-pointer)))
	 (major 0)
	 (minor 0))
    (cffi:with-foreign-slots ((xcb-glx::major-version xcb-glx::minor-version)
			      r
			      (:struct xcb-glx:query-version-reply-data))
      (setf major xcb-glx::major-version
	    minor xcb-glx::minor-version)
      (cffi:foreign-free r)
      (values major minor))))

(let* ((connection (xcb:default-connection))
       (screen (xcb-util:xcb-aux-get-screen connection 0))
       fbconfig fid vid)
  (declare (ignore screen))
  (setf fbconfig (xcb:find-fb-config-by-attributes connection
						   0
						   `((,xcb-glx::fb-doublebuffer . 1)
						     (,xcb-glx:fb-stencil-size . 8)
						     (,xcb-glx:fb-red-size . 8)
						     (,xcb-glx:fb-green-size . 8)
						     (,xcb-glx:fb-blue-size . 8)
						     (,xcb-glx:fb-alpha-size . 8)
						     (,xcb-glx:fb-depth-size . 24)
						     (,xcb-glx:fb-buffer-size . 32)
						     (,xcb-glx:fb-render-type . ,xcb-glx:fb-rgba-bit)
						     (,xcb-glx:fb-drawable-type . ,(logior xcb-glx:fb-window-bit
											   xcb-glx:fb-pixmap-bit
											   xcb-glx:fb-pbuffer-bit))
						     (,xcb-glx:fb-x-renderable . 1))))
  (setf fid (xcb:find-fb-attr-array fbconfig xcb-glx::fb-fbconfig-id))
  (setf vid (xcb:find-fb-attr-array fbconfig xcb-glx::fb-visual-id))
  (xcb-core:disconnect connection)
  (print fbconfig)
  (format t "choosing fbconfig id 0x~x with visual id 0x~x ~a"
	  fid
	  vid
	  (terpri)))
; choosing fbconfig id 0x122 with visual id 0x31
