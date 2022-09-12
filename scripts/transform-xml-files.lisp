(ql:quickload :xmls)

(defparameter filename #P"/usr/local/src/xcb-proto/src/glx.xml")

(defparameter xproto (xmls:parse (uiop:read-file-string filename)))

(ql:quickload :str)

(defun add-prefix (s)
  (str:concat "xcb-glx-" s))

(defparameter enums nil)

(defun filter-item-nodes (nodes)
  (remove-if-not (lambda (node)
		   (string-equal "item" (xmls:node-name node)))
		 (xmls:node-children nodes)))

(defun extract-enum-item-info (enum)
  (map 'list
       (lambda (item)
	 (let ((item-name (xmls:node-attrs item)))
	   (cons (str:param-case (cadar item-name))
		 (xmls:node-children (car (xmls:node-children item))))))
       (filter-item-nodes enum)))

(setf enums (map 'list (lambda (enum)
			 (let* ((enum-name (xmls:node-attrs enum)))
			   (list (str:param-case (cadar enum-name))
				 (extract-enum-item-info enum))))
		 (xmls:xmlrep-find-child-tags "enum" xproto)))

(defun make-enum-opt (enum-name)
  (flet ((to-opt-name (item)
	   (add-prefix (str:concat enum-name "-" (car item)))))
   (lambda (acc item)
     (str:concat acc
		 (string #\NEWLINE)
		 "  ("
		 (to-opt-name item)
		 " "
		 (cadr item)
		 ")"))))

(let ((all (reduce (lambda (acc enum)
		     (let* ((enum-name (car enum))
			    (enum-opts (cadr enum))
			    (f (make-enum-opt enum-name)))
		       (str:concat acc
				   "(cffi:defcenum "
				   (add-prefix enum-name)
				   (reduce f (cdr enum-opts) :initial-value (funcall f "" (car enum-opts)))
				   ")"
				   (string #\NEWLINE)
				   (string #\NEWLINE))))
		   enums
		   :initial-value "")))
  (print all))

(defparameter idtypes nil)

(setf idtypes
      (map 'list
	   (lambda (item)
	     (cadr (assoc "name" (xmls:node-attrs item) :test #'string-equal)) )
	   (xmls:xmlrep-find-child-tags "xidtype" xproto)))

(flet ((definetype (item)
	 (str:concat "(cffi:defctype " (add-prefix (str:param-case item)) " :uint32)")))
 (let ((all (reduce (lambda (acc item)
		      (str:concat acc (string #\NEWLINE) (definetype item)))
		    (cdr idtypes)
		    :initial-value (definetype (car idtypes)))))
   (print all)))
