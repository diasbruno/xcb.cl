;;;; xcb-glx.lisp

(in-package #:xcb-glx)

(cffi:define-foreign-library xcb-glx
  (:unix "/nix/store/xrdm5xqqvp3141iaccz9a2lp65qa5kr4-libxcb-1.14/lib/libxcb-glx.so"))

(cffi:load-foreign-library 'xcb-glx)

(cffi:defcenum fb-config-render-type
  (fb-rgba-bit                    #x00000001)
  (fb-color-index-bit             #x00000002)
  (fb-pbuffer-clobber-mask        #x08000000)
  (fb-front-left-buffer-bit       #x00000001)
  (fb-front-right-buffer-bit      #x00000002)
  (fb-back-left-buffer-bit        #x00000004)
  (fb-back-right-buffer-bit       #x00000008)
  (fb-aux-buffers-bit             #x00000010)
  (fb-depth-buffer-bit            #x00000020)
  (fb-stencil-buffer-bit          #x00000040)
  (fb-accum-buffer-bit            #x00000080))

(cffi:defcenum dk2
  (fb-vendor                      #x1)
  (fb-version                     #x2)
  (fb-extensions                  #x3))

(cffi:defcenum fb-config-drawable-type
  (fb-window-bit                  #x00000001)
  (fb-pixmap-bit                  #x00000002)
  (fb-pbuffer-bit                 #x00000004))

(cffi:defcenum fb-config-attrs
  (fb-use-gl                      1)  ;; all
  (fb-buffer-size                 2)
  (fb-level                       3)
  (fb-rgba                        4)
  (fb-doublebuffer                5)
  (fb-stereo                      6)
  (fb-aux-buffers                 7)
  (fb-red-size                    8)
  (fb-green-size                  9)
  (fb-blue-size                   10)
  (fb-alpha-size                  11)
  (fb-depth-size                  12)
  (fb-stencil-size                13)
  (fb-accum-red-size              14)
  (fb-accum-green-size            15)
  (fb-accum-blue-size             16)
  (fb-accum-alpha-size            17)
  (fb-config-caveat               #x20) ;; =>1.3
  (fb-x-visual-type               #x22)
  (fb-transparent-type            #x23)
  (fb-transparent-index-value     #x24)
  (fb-transparent-red-value       #x25)
  (fb-transparent-green-value     #x26)
  (fb-transparent-blue-value      #x27)
  (fb-transparent-alpha-value     #x28)
  (fb-dont-care                   #xffffffff)
  (fb-none                        #x8000)
  (fb-slow-config                 #x8001)
  (fb-true-color                  #x8002)
  (fb-direct-color                #x8003)
  (fb-pseudo-color                #x8004)
  (fb-static-color                #x8005)
  (fb-gray-scale                  #x8006)
  (fb-static-gray                 #x8007)
  (fb-transparent-rgb             #x8008)
  (fb-transparent-index           #x8009)
  (fb-visual-id                   #x800b)
  (fb-screen                      #x800c)
  (fb-non-conformant-config       #x800d)
  (fb-drawable-type               #x8010)
  (fb-render-type                 #x8011)
  (fb-x-renderable                #x8012)
  (fb-fbconfig-id                 #x8013)
  (fb-rgba-type                   #x8014)
  (fb-color-index-type            #x8015)
  (fb-max-pbuffer-width           #x8016)
  (fb-max-pbuffer-height          #x8017)
  (fb-max-pbuffer-pixels          #x8018)
  (fb-preserved-contents          #x801b)
  (fb-largest-pbuffer             #x801c)
  (fb-width                       #x801d)
  (fb-height                      #x801e)
  (fb-event-mask                  #x801f)
  (fb-damaged                     #x8020)
  (fb-saved                       #x8021)
  (fb-window                      #x8022)
  (fb-pbuffer                     #x8023)
  (fb-pbuffer-height              #x8040)
  (fb-pbuffer-width               #x8041))

(cffi:defctype pixmap :uint32)
(cffi:defctype pbuffer :uint32)
(cffi:defctype context :uint32)
(cffi:defctype window :uint32)
(cffi:defctype fbconfig :uint32)
(cffi:defctype drawable :uint32)
(cffi:defctype context-tag :uint32)
(cffi:defctype make-current-cookie :uint32)
(cffi:defctype make-context-current-cookie :uint32)
(cffi:defctype query-version-cookie :uint32)
(cffi:defctype get-fb-configs-cookie :uint32)
(cffi:defctype query-context-cookie :uint32)

(cffi:defcenum pbcet
  (pbcet-damaged 32791)
  (pbcet-saved 32792))

(cffi:defcenum pbcdt
  (pbcdt-window 32793)
  (pbcdt-pbuffer 32794))

(cffi:defcenum gc
  (gc-gl-current-bit 0)
  (gc-gl-point-bit 1)
  (gc-gl-line-bit 2)
  (gc-gl-polygon-bit 3)
  (gc-gl-polygon-stipple-bit 4)
  (gc-gl-pixel-mode-bit 5)
  (gc-gl-lighting-bit 6)
  (gc-gl-fog-bit 7)
  (gc-gl-depth-buffer-bit 8)
  (gc-gl-accum-buffer-bit 9)
  (gc-gl-stencil-buffer-bit 10)
  (gc-gl-viewport-bit 11)
  (gc-gl-transform-bit 12)
  (gc-gl-enable-bit 13)
  (gc-gl-color-buffer-bit 14)
  (gc-gl-hint-bit 15)
  (gc-gl-eval-bit 16)
  (gc-gl-list-bit 17)
  (gc-gl-texture-bit 18)
  (gc-gl-scissor-bit 19)
  (gc-gl-all-attrib-bits 16777215))

(cffi:defcenum rm
  (rm-gl-render 7168)
  (rm-gl-feedback 7169)
  (rm-gl-select 7170))

(cffi:defcstruct make-context-current-reply-data
  (response_type :uint8)
  (pad0 :uint8)
  (sequence :uint16)
  (length :uint32)
  (context-tag context-tag)
  (pad1 :uint8 :count 20))

(cffi:defcstruct query-version-reply-data
  (response-type :uint8)
  (pad0 :uint8)
  (sequence :uint16)
  (length :uint32)
  (major-version :uint32)
  (minor-version :uint32)
  (pad1 :uint8 :count 16))

(cffi:defcfun ("xcb_glx_create_window" create-window)
    xcb-core:void-cookie
  (connection xcb-core::connection)
  (screen-num :uint32)
  (fbconfig fbconfig)
  (window xcb-core:window)
  (glx-window window)
  (num-attrs :uint32)
  (attrs :pointer))

(cffi:defcfun ("xcb_glx_make_context_current" make-context-current)
    make-context-current-cookie
  (connection xcb-core::connection)
  (tag context)
  (drawable drawable)
  (context drawable)
  (old-context context))

(cffi:defcfun ("xcb_glx_make_context_current_reply" make-context-current-reply)
    (:pointer (:struct make-context-current-reply-data))
  (connection xcb-core::connection)
  (cookie make-context-current-cookie)
  (error-ref :pointer))

(cffi:defcfun ("xcb_glx_create_new_context" create-new-context)
    xcb-core:void-cookie
  (connection xcb-core::connection)
  (context context)
  (fbconfig fbconfig)
  (screen-num :uint32)
  (render-type :uint32)
  (share context)
  (is-direct :uint8))

(cffi:defcfun ("xcb_glx_query_version" query-version)
    query-version-cookie
  (connection xcb-core::connection)
  (major-version :uint32)
  (minor-version :uint32))

(cffi:defcfun ("xcb_glx_query_version_reply" query-version-reply)
    (:pointer (:struct query-version-reply-data))
  (connection xcb-core::connection)
  (cookie query-version-cookie)
  (error-ref :pointer))

(cffi:defcstruct get-fb-configs-reply-data
  (response-type :uint8)
  (pad0 :uint8)
  (sequence :uint16)
  (length :uint32)
  (num-fb-configs :uint32)
  (num-properties :uint32)
  (pad1 :uint8 :count 16))

(cffi:defctype get-fb-configs-cookie :uint32)

(cffi:defcfun ("xcb_glx_get_fb_configs" get-fb-configs)
    get-fb-configs-cookie
  (connection xcb-core::connection)
  (screen-num :uint32))

(cffi:defcfun ("xcb_glx_get_fb_configs_reply" get-fb-configs-reply)
    (:pointer (:struct get-fb-configs-reply-data))
  (connection xcb-core::connection)
  (cookie get-fb-configs-cookie)
  (error-ref :pointer))

(cffi:defcfun ("xcb_glx_get_fb_configs_property_list" get-fb-configs-property-list)
    (:pointer :uint32)
  (fb-config-reply (:pointer (:struct get-fb-configs-reply-data))))

(cffi:defcfun ("xcb_glx_query_context" query-context)
    query-context-cookie
  (connection xcb-core::connection)
  (context context))

(cffi:defcstruct query-context-reply-data
  (response-type :uint8)
  (pad0 :uint8)
  (sequence :uint16)
  (length :uint32)
  (num-attribs :uint32)
  (pad1 :uint8 :count 20))

(cffi:defcfun ("xcb_glx_query_context_reply" query-context-reply)
    query-context-reply-data
  (connection xcb-core::connection)
  (cookie query-context-cookie)
  (error-ref :pointer))
