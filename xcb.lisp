;;;; xcb.lisp

(in-package #:xcb)

(defun default-connection ()
  (xcb-core:connect (cffi:null-pointer)
		    (cffi:null-pointer)))

(defun create-window (connection
		      screen
		      window
		      x y w h b
		      klass
		      glx-visual-id
		      mask
		      attrs)
  (let ((value-list (cffi:foreign-alloc :uint32
					:initial-contents attrs
					:count (length attrs))))
    (xcb-core:create-window connection
			    (xcb-core:screen->root-depth screen)
			    window
			    (xcb-core:screen->root screen)
			    x y h w b
			    klass
			    glx-visual-id
			    mask
			    value-list)
    (cffi:foreign-free value-list)))

(defun create-colormap (connection
			allocation
			colormap
			screen
			glx-visual-id)
 (xcb-core:create-colormap connection
			   allocation
			   colormap
			   (xcb-core:screen->root screen)
			   glx-visual-id))

(defun glx-create-new-context (connection
			       glxcontext
			       fb-config
			       screennum
			       &key
			       (render-type xcb-glx::fb-rgba-type))
  (xcb-glx:create-new-context connection
			      glxcontext
			      (find-fb-attr-array fb-config
						  xcb-glx::fb-fbconfig-id)
			      screennum
			      render-type
			      0
			      1))

(defun find-by-attr (props num-properties offset attr)
  "find an attributes ATTR in a list of properties PROPS pointer reference
 limited by NUM-PROPERTIES at OFFSET position."
  (block nil
    (loop :for j :from 0 :to num-properties
	  :when (and (evenp j)
		     (equal attr (cffi:mem-aref props :uint32 (+ offset j))))
	    :do (return-from nil
		    (cffi:mem-aref props :uint32 (+ offset j 1))))))

(defun fb-config-has-invalid-attrs (props num-properties offset attributes)
  "check if all ATTRIBUTES are in a framebuffer configuration as PROPS
 from OFFSET limited by NUM-PROPERTIES"
  (block nil
    (loop :for (attr . value) :in attributes
	  :do (when (not (equal value
				(find-by-attr props num-properties offset attr)))
		(return-from nil t)))))

(defun find-fb-config-by-attributes (connection screen-num attributes)
  "find a validf framebuffer configuration by a list of ATTRIBUTES
 for CONNECTION and SCREEN-NUM."
  (block found-fbconfig
    (let* ((rpl (xcb-glx:get-fb-configs connection screen-num))
	   (cfgs (xcb-glx:get-fb-configs-reply connection rpl (cffi:null-pointer)))
	   (props (xcb-glx:get-fb-configs-property-list cfgs)))
      (cffi:with-foreign-slots ((xcb-glx::length
				 xcb-glx::num-fb-configs
				 xcb-glx::num-properties)
				cfgs
				(:struct xcb-glx:get-fb-configs-reply-data))
	(let ((num-properties (* 2 xcb-glx::num-properties))
	      (offset 0))
	  (loop :for i :from 0 :to xcb-glx::num-fb-configs
		:do (when (evenp i)
		      (if (fb-config-has-invalid-attrs props num-properties offset attributes)
			  (setf offset (+ offset num-properties))
			  (progn
			    (cffi:foreign-free cfgs)
			    (return-from found-fbconfig
			      (convert-fb-config-prop-to-array props num-properties offset)))))))))))

(defun find-fb-by-attr (fb-config attribute)
  "find an ATTRIBUTE on a FB-CONFIG."
  (block nil
    (loop :for x :below (length fb-config)
	  :when (and (evenp x) (equal attribute (aref fb-config x)))
	    :do (return-from nil (aref fb-config (1+ x))))))

(defun convert-fb-config-prop-to-array (props num-properties offset)
  "convert framebuffer configuration as PROPS from OFFSET limited
 by NUM-PROPERTIES."
  (block nil
    (loop
      :with total = (1- num-properties)
      :with arr = (make-array num-properties :element-type 'integer)
      :for c :from 0 :to total
      :do (if (< c total)
	      (setf (aref arr c)
		    (cffi:mem-aref props :uint32 (+ offset c)))
	      (return-from nil arr)))))
