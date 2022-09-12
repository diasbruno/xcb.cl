;;;; xcb-core.lisp

(in-package #:xcb-core)

(cffi:define-foreign-library xcb
  (:unix "/nix/store/xrdm5xqqvp3141iaccz9a2lp65qa5kr4-libxcb-1.14/lib/libxcb.so"))

(cffi:load-foreign-library 'xcb)

(cffi:defctype connection :pointer)

(cffi:defctype window :uint32)
(cffi:defctype pixmap :uint32)
(cffi:defctype cursor :uint32)
(cffi:defctype font :uint32)
(cffi:defctype gcontext :uint32)
(cffi:defctype colormap :uint32)
(cffi:defctype atom :uint32)
(cffi:defctype visualid :uint32)

(cffi:defctype void-cookie :uint32)

(cffi:defcstruct screen
  (root window)
  (default-colormap colormap)
  (white-pixel :uint32)
  (black-pixel :uint32)
  (current-input-mask :uint32)
  (width-in-pixels :uint16)
  (height-in-pixels :uint16)
  (width-in-millimiters :uint16)
  (height-in-millimiters :uint16)
  (min-installed-maps :uint16)
  (max-installed-maps :uint16)
  (root-visualid visualid)
  (backing-store :uint8)
  (save-unders :uint8)
  (root-depth :uint8)
  (allowed-depth-len :uint8))

(cffi:defcstruct generic-error
  (response-type :uint8)
  (error-code :uint8)
  (sequence :uint16)
  (resource-id :uint32)
  (minor-code :uint16)
  (major-code :uint8)
  (pad0 :uint8)
  (pad :uint32 :count 5)
  (full-sequence :uint32))

(cffi:defcenum connection-error
  "conn-error - xcb connection errors because of socket, pipe and other stream errors.
conn-closed-ext-notsupported - xcb connection shutdown because of extension not supported
conn-closed-mem-insufficient - malloc(), calloc() and realloc() error upon failure, for eg ENOMEM
conn-closed-req-len-exceed - Connection closed, exceeding request length that server accepts.
conn-closed-parse-err - Connection closed, error during parsing display string.
conn-closed-invalid-screen - Connection closed because the server does not have a screen matching the display.
conn-closed-fdpassing-failed - Connection closed because some FD passing operation failed"
  (conn-ok 0)
  (conn-error 1)
  (conn-closed-ext-notsupported 2)
  (conn-closed-mem-insufficient 3)
  (conn-closed-req-len-exceed 4)
  (conn-closed-parse-err 5)
  (conn-closed-invalid-screen 6)
  (conn-closed-fdpassing-failed 7))

(cffi:defcenum visual-class
  (visual-class-static-gray 0)
  (visual-class-gray-scale 1)
  (visual-class-static-color 2)
  (visual-class-pseudo-color 3)
  (visual-class-true-color 4)
  (visual-class-direct-color 5))

(cffi:defcenum event-mask
  (event-mask-no-event 0)
  (event-mask-key-press 0)
  (event-mask-key-release 1)
  (event-mask-button-press 2)
  (event-mask-button-release 3)
  (event-mask-enter-window 4)
  (event-mask-leave-window 5)
  (event-mask-pointer-motion 6)
  (event-mask-pointer-motion-hint 7)
  (event-mask-button1-motion 8)
  (event-mask-button2-motion 9)
  (event-mask-button3-motion 10)
  (event-mask-button4-motion 11)
  (event-mask-button5-motion 12)
  (event-mask-button-motion 13)
  (event-mask-keymap-state 14)
  (event-mask-exposure 15)
  (event-mask-visibility-change 16)
  (event-mask-structure-notify 17)
  (event-mask-resize-redirect 18)
  (event-mask-substructure-notify 19)
  (event-mask-substructure-redirect 20)
  (event-mask-focus-change 21)
  (event-mask-property-change 22)
  (event-mask-color-map-change 23)
  (event-mask-owner-grab-button 24))

(cffi:defcenum backing-store
  (backing-store-not-useful 0)
  (backing-store-when-mapped 1)
  (backing-store-always 2))

(cffi:defcenum image-order
  (image-order-lsb-first 0)
  (image-order-msb-first 1))

(cffi:defcenum mod-mask
  (mod-mask-shift 0)
  (mod-mask-lock 1)
  (mod-mask-control 2)
  (mod-mask-1 3)
  (mod-mask-2 4)
  (mod-mask-3 5)
  (mod-mask-4 6)
  (mod-mask-5 7)
  (mod-mask-any 15))

(cffi:defcenum key-but-mask
  (key-but-mask-shift 0)
  (key-but-mask-lock 1)
  (key-but-mask-control 2)
  (key-but-mask-mod1 3)
  (key-but-mask-mod2 4)
  (key-but-mask-mod3 5)
  (key-but-mask-mod4 6)
  (key-but-mask-mod5 7)
  (key-but-mask-button1 8)
  (key-but-mask-button2 9)
  (key-but-mask-button3 10)
  (key-but-mask-button4 11)
  (key-but-mask-button5 12))

(cffi:defcenum window-unused
  (window-none 0))

(cffi:defcenum button-mask
  (button-mask-1 8)
  (button-mask-2 9)
  (button-mask-3 10)
  (button-mask-4 11)
  (button-mask-5 12)
  (button-mask-any 15))

(cffi:defcenum motion
  (motion-normal 0)
  (motion-hint 1))

(cffi:defcenum notify-detail
  (notify-detail-ancestor 0)
  (notify-detail-virtual 1)
  (notify-detail-inferior 2)
  (notify-detail-nonlinear 3)
  (notify-detail-nonlinear-virtual 4)
  (notify-detail-pointer 5)
  (notify-detail-pointer-root 6)
  (notify-detail-none 7))

(cffi:defcenum notify-mode
  (notify-mode-normal 0)
  (notify-mode-grab 1)
  (notify-mode-ungrab 2)
  (notify-mode-while-grabbed 3))

(cffi:defcenum visibility
  (visibility-unobscured 0)
  (visibility-partially-obscured 1)
  (visibility-fully-obscured 2))

(cffi:defcenum place
  (place-on-top 0)
  (place-on-bottom 1))

(cffi:defcenum property
  (property-new-value 0)
  (property-delete 1))

(cffi:defcenum time
  (time-current-time 0))

(cffi:defcenum atom
  (atom-none 0)
  (atom-any 0)
  (atom-primary 1)
  (atom-secondary 2)
  (atom-arc 3)
  (atom-atom 4)
  (atom-bitmap 5)
  (atom-cardinal 6)
  (atom-colormap 7)
  (atom-cursor 8)
  (atom-cut-buffer0 9)
  (atom-cut-buffer1 10)
  (atom-cut-buffer2 11)
  (atom-cut-buffer3 12)
  (atom-cut-buffer4 13)
  (atom-cut-buffer5 14)
  (atom-cut-buffer6 15)
  (atom-cut-buffer7 16)
  (atom-drawable 17)
  (atom-font 18)
  (atom-integer 19)
  (atom-pixmap 20)
  (atom-point 21)
  (atom-rectangle 22)
  (atom-resource-manager 23)
  (atom-rgb-color-map 24)
  (atom-rgb-best-map 25)
  (atom-rgb-blue-map 26)
  (atom-rgb-default-map 27)
  (atom-rgb-gray-map 28)
  (atom-rgb-green-map 29)
  (atom-rgb-red-map 30)
  (atom-string 31)
  (atom-visualid 32)
  (atom-window 33)
  (atom-wm-command 34)
  (atom-wm-hints 35)
  (atom-wm-client-machine 36)
  (atom-wm-icon-name 37)
  (atom-wm-icon-size 38)
  (atom-wm-name 39)
  (atom-wm-normal-hints 40)
  (atom-wm-size-hints 41)
  (atom-wm-zoom-hints 42)
  (atom-min-space 43)
  (atom-norm-space 44)
  (atom-max-space 45)
  (atom-end-space 46)
  (atom-superscript-x 47)
  (atom-superscript-y 48)
  (atom-subscript-x 49)
  (atom-subscript-y 50)
  (atom-underline-position 51)
  (atom-underline-thickness 52)
  (atom-strikeout-ascent 53)
  (atom-strikeout-descent 54)
  (atom-italic-angle 55)
  (atom-x-height 56)
  (atom-quad-width 57)
  (atom-weight 58)
  (atom-point-size 59)
  (atom-resolution 60)
  (atom-copyright 61)
  (atom-notice 62)
  (atom-font-name 63)
  (atom-family-name 64)
  (atom-full-name 65)
  (atom-cap-height 66)
  (atom-wm-class 67)
  (atom-wm-transient-for 68))

(cffi:defcenum colormap-state
  (colormap-state-uninstalled 0)
  (colormap-state-installed 1))

(cffi:defcenum colormap
  (colormap-none 0))

(cffi:defcenum mapping
  (mapping-modifier 0)
  (mapping-keyboard 1)
  (mapping-pointer 2))

(cffi:defcenum window-class
  (window-class-copy-from-parent 0)
  (window-class-input-output 1)
  (window-class-input-only 2))

(cffi:defcenum cw
  (cw-back-pixmap 0)
  (cw-back-pixel 1)
  (cw-border-pixmap 2)
  (cw-border-pixel 3)
  (cw-bit-gravity 4)
  (cw-win-gravity 5)
  (cw-backing-store 6)
  (cw-backing-planes 7)
  (cw-backing-pixel 8)
  (cw-override-redirect 9)
  (cw-save-under 10)
  (cw-event-mask 11)
  (cw-dont-propagate 12)
  (cw-colormap 13)
  (cw-cursor 14))

(cffi:defcenum back-pixmap
  (back-pixmap-none 0)
  (back-pixmap-parent-relative 1))

(cffi:defcenum gravity
  (gravity-bit-forget 0)
  (gravity-win-unmap 0)
  (gravity-north-west 1)
  (gravity-north 2)
  (gravity-north-east 3)
  (gravity-west 4)
  (gravity-center 5)
  (gravity-east 6)
  (gravity-south-west 7)
  (gravity-south 8)
  (gravity-south-east 9)
  (gravity-static 10))

(cffi:defcenum map-state
  (map-state-unmapped 0)
  (map-state-unviewable 1)
  (map-state-viewable 2))

(cffi:defcenum set-mode
  (set-mode-insert 0)
  (set-mode-delete 1))

(cffi:defcenum config-window
  (config-window-x 0)
  (config-window-y 1)
  (config-window-width 2)
  (config-window-height 3)
  (config-window-border-width 4)
  (config-window-sibling 5)
  (config-window-stack-mode 6))

(cffi:defcenum stack-mode
  (stack-mode-above 0)
  (stack-mode-below 1)
  (stack-mode-top-if 2)
  (stack-mode-bottom-if 3)
  (stack-mode-opposite 4))

(cffi:defcenum circulate
  (circulate-raise-lowest 0)
  (circulate-lower-highest 1))

(cffi:defcenum prop-mode
  (prop-mode-replace 0)
  (prop-mode-prepend 1)
  (prop-mode-append 2))

(cffi:defcenum get-property-type
  (get-property-type-any 0))

(cffi:defcenum send-event-dest
  (send-event-dest-pointer-window 0)
  (send-event-dest-item-focus 1))

(cffi:defcenum grab-mode
  (grab-mode-sync 0)
  (grab-mode-async 1))

(cffi:defcenum grab-status
  (grab-status-success 0)
  (grab-status-already-grabbed 1)
  (grab-status-invalid-time 2)
  (grab-status-not-viewable 3)
  (grab-status-frozen 4))

(cffi:defcenum cursor
  (cursor-none 0))

(cffi:defcenum button-index
  (button-index-any 0)
  (button-index-1 1)
  (button-index-2 2)
  (button-index-3 3)
  (button-index-4 4)
  (button-index-5 5))

(cffi:defcenum grab
  (grab-any 0))

(cffi:defcenum allow
  (allow-async-pointer 0)
  (allow-sync-pointer 1)
  (allow-replay-pointer 2)
  (allow-async-keyboard 3)
  (allow-sync-keyboard 4)
  (allow-replay-keyboard 5)
  (allow-async-both 6)
  (allow-sync-both 7))

(cffi:defcenum input-focus
  (input-focus-none 0)
  (input-focus-pointer-root 1)
  (input-focus-parent 2)
  (input-focus-follow-keyboard 3))

(cffi:defcenum font-draw
  (font-draw-left-to-right 0)
  (font-draw-right-to-left 1))

(cffi:defcenum gc
  (gc-function 0)
  (gc-plane-mask 1)
  (gc-foreground 2)
  (gc-background 3)
  (gc-line-width 4)
  (gc-line-style 5)
  (gc-cap-style 6)
  (gc-join-style 7)
  (gc-fill-style 8)
  (gc-fill-rule 9)
  (gc-tile 10)
  (gc-stipple 11)
  (gc-tile-stipple-origin-x 12)
  (gc-tile-stipple-origin-y 13)
  (gc-font 14)
  (gc-subwindow-mode 15)
  (gc-graphics-exposures 16)
  (gc-clip-origin-x 17)
  (gc-clip-origin-y 18)
  (gc-clip-mask 19)
  (gc-dash-offset 20)
  (gc-dash-list 21)
  (gc-arc-mode 22))

(cffi:defcenum gx
  (gx-clear 0)
  (gx-and 1)
  (gx-and-reverse 2)
  (gx-copy 3)
  (gx-and-inverted 4)
  (gx-noop 5)
  (gx-xor 6)
  (gx-or 7)
  (gx-nor 8)
  (gx-equiv 9)
  (gx-invert 10)
  (gx-or-reverse 11)
  (gx-copy-inverted 12)
  (gx-or-inverted 13)
  (gx-nand 14)
  (gx-set 15))

(cffi:defcenum line-style
  (line-style-solid 0)
  (line-style-on-off-dash 1)
  (line-style-double-dash 2))

(cffi:defcenum cap-style
  (cap-style-not-last 0)
  (cap-style-butt 1)
  (cap-style-round 2)
  (cap-style-projecting 3))

(cffi:defcenum join-style
  (join-style-miter 0)
  (join-style-round 1)
  (join-style-bevel 2))

(cffi:defcenum fill-style
  (fill-style-solid 0)
  (fill-style-tiled 1)
  (fill-style-stippled 2)
  (fill-style-opaque-stippled 3))

(cffi:defcenum fill-rule
  (fill-rule-even-odd 0)
  (fill-rule-winding 1))

(cffi:defcenum subwindow-mode
  (subwindow-mode-clip-by-children 0)
  (subwindow-mode-include-inferiors 1))

(cffi:defcenum arc-mode
  (arc-mode-chord 0)
  (arc-mode-pie-slice 1))

(cffi:defcenum clip-ordering
  (clip-ordering-unsorted 0)
  (clip-ordering-y-sorted 1)
  (clip-ordering-yx-sorted 2)
  (clip-ordering-yx-banded 3))

(cffi:defcenum coord-mode
  (coord-mode-origin 0)
  (coord-mode-previous 1))

(cffi:defcenum poly-shape
  (poly-shape-complex 0)
  (poly-shape-nonconvex 1)
  (poly-shape-convex 2))

(cffi:defcenum image-format
  (image-format-xy-bitmap 0)
  (image-format-xy-pixmap 1)
  (image-format-z-pixmap 2))

(cffi:defcenum colormap-alloc
  (colormap-alloc-none 0)
  (colormap-alloc-all 1))

(cffi:defcenum color-flag
  (color-flag-red 0)
  (color-flag-green 1)
  (color-flag-blue 2))

(cffi:defcenum pixmap
  (pixmap-none 0))

(cffi:defcenum font
  (font-none 0))

(cffi:defcenum query-shape-of
  (query-shape-of-largest-cursor 0)
  (query-shape-of-fastest-tile 1)
  (query-shape-of-fastest-stipple 2))

(cffi:defcenum kb
  (kb-key-click-percent 0)
  (kb-bell-percent 1)
  (kb-bell-pitch 2)
  (kb-bell-duration 3)
  (kb-led 4)
  (kb-led-mode 5)
  (kb-key 6)
  (kb-auto-repeat-mode 7))

(cffi:defcenum led-mode
  (led-mode-off 0)
  (led-mode-on 1))

(cffi:defcenum auto-repeat-mode
  (auto-repeat-mode-off 0)
  (auto-repeat-mode-on 1)
  (auto-repeat-mode-default 2))

(cffi:defcenum blanking
  (blanking-not-preferred 0)
  (blanking-preferred 1)
  (blanking-default 2))

(cffi:defcenum exposures
  (exposures-not-allowed 0)
  (exposures-allowed 1)
  (exposures-default 2))

(cffi:defcenum host-mode
  (host-mode-insert 0)
  (host-mode-delete 1))

(cffi:defcenum family
  (family-internet 0)
  (family-de-cnet 1)
  (family-chaos 2)
  (family-server-interpreted 5)
  (family-internet6 6))

(cffi:defcenum access-control
  (access-control-disable 0)
  (access-control-enable 1))

(cffi:defcenum close-down
  (close-down-destroy-all 0)
  (close-down-retain-permanent 1)
  (close-down-retain-temporary 2))

(cffi:defcenum kill
  (kill-all-temporary 0))

(cffi:defcenum screen-saver
  (screen-saver-reset 0)
  (screen-saver-active 1))

(cffi:defcenum mapping-status
  (mapping-status-success 0)
  (mapping-status-busy 1)
  (mapping-status-failure 2))

(cffi:defcenum map-index
  (map-index-shift 0)
  (map-index-lock 1)
  (map-index-control 2)
  (map-index-1 3)
  (map-index-2 4)
  (map-index-3 5)
  (map-index-4 6)
  (map-index-5 7))

(cffi:defcfun ("xcb_connect" connect) :pointer
  "Connects to the X server specified by @p displayname. If @p
  displayname is @c NULL, uses the value of the DISPLAY environment
  variable. If a particular screen on that server is preferred, the
  int pointed to by @p screenp (if not @c NULL) will be set to that
  screen; otherwise the screen will be set to 0.

  Always returns a non-NULL pointer to a xcb_connection_t, even on failure.
  Callers need to use xcb_connection_has_error() to check for failure.
  When finished, use xcb_disconnect() to close the connection and free
  the structure."
  (displayname :string)
  (screen (:pointer :int)))

(cffi:defcfun ("xcb_disconnect" disconnect) :void
  "Closes the file descriptor and frees all memory associated with the
  connection @c c. If @p c is @c NULL, nothing is done."
  (connection :pointer))

(cffi:defcfun ("xcb_connection_has_error" connection-has-error) connection-error
  "Some errors that occur in the context of an xcb_connection_t
  are unrecoverable. When such an error occurs, the
  connection is shut down and further operations on the
  xcb_connection_t have no effect, but memory will not be freed until
  xcb_disconnect() is called on the xcb_connection_t."
  (connection :pointer))

(cffi:defcfun ("xcb_get_setup" get-setup) :pointer
  "Accessor for the data returned by the server when the xcb_connection_t
was initialized. This data includes:
  - the server's required format for images,
  - a list of available visuals,
  - a list of available screens,
  - the server's maximum request length (in the absence of the BIG-REQUESTS extension),
  - and other assorted information.

See the X protocol specification for more details.

The result must not be freed."
  (connection :pointer))

(cffi:defcfun ("xcb_generate_id" generate-id) :uint32
  "Allocates an XID for a new object. Typically used just prior to
various object creation functions, such as xcb_create_window."
  (connection :pointer))

(cffi:defcfun ("xcb_create_window" create-window) void-cookie
  (connection :pointer)
  (depth :uint8)
  (window window)
  (parent-window window)
  (x :uint16)
  (y :uint16)
  (width :uint16)
  (height :uint16)
  (border :uint16)
  (class window-class)
  (visualid :uint32)
  (value-mask :uint32)
  (value-list :pointer))

(cffi:defcfun ("xcb_map_window" map-window)
    void-cookie
  (connection :pointer)
  (window window))

(cffi:defcfun ("xcb_unmap_window" unmap-window)
    void-cookie
  (connection :pointer)
  (window window))

(cffi:defcfun ("xcb_flush" flush) :int
  (connection :pointer))

(cffi:defcfun ("xcb_create_colormap" create-colormap)
    void-cookie
  (connection :pointer)
  (alloc :uint8)
  (colormapid colormap)
  (window window)
  (visual visualid))

(defun screen->root (screen)
  (cffi:foreign-slot-value screen '(:struct screen) 'root))

(defun screen->root-visualid (screen)
  (cffi:foreign-slot-value screen '(:struct screen) 'root-visualid))

(defun screen->root-depth (screen)
  (cffi:foreign-slot-value screen '(:struct screen) 'root-depth))

(defun screen->white-pixel (screen)
  (cffi:foreign-slot-value screen '(:struct screen) 'white-pixel))
