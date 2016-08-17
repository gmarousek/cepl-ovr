;;;; cepl-ovr.lisp
(in-package #:cepl-ovr)
;;;; ****************************START OF CEPL CODE******************************

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)
(defparameter *entities* nil)

(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
	  (col vert)))

(defun-g frag ((color :vec4))
  color)

(def-g-> prog-1 ()
  #'vert #'frag)

(defun triangle (p1x p1y p1z p2x p2y p2z p3x p3y p3z)
  (list (list (v! p1x p1y p1z) (v! 0 1 0 1))
  	(list (v! p2x p2y p2z) (v! 0 0 1 1))
  	(list (v! p3x p3y p3z) (v! 1 0 0 1))))

(defun plane (p1x p1y p1z p2x p2y p2z p3x p3y p3z p4x p4y p4z)
  (append (triangle p1x p1y p1z p2x p2y p2z p3x p3y p3z)
	  (triangle p3x p3y p3z p2x p2y p2z p4x p4y p4z)))

(defun box (x-size y-size z-size)
  (append (plane
	   (- (/ x-size 2)) (/ y-size 2) (/ z-size 2)
	   (- (/ x-size 2)) (- (/ y-size 2)) (/ z-size 2)
	   (/ x-size 2) (/ y-size 2) (/ z-size 2)
	   (/ x-size 2) (- (/ y-size 2)) (/ z-size 2))
	  (plane
	   (/ x-size 2) (/ y-size 2) (/ z-size 2)
	   (/ x-size 2) (-(/ y-size 2)) (/ z-size 2)
	   (/ x-size 2) (/ y-size 2) (- (/ z-size 2))
	   (/ x-size 2) (- (/ y-size 2)) (- (/ z-size 2)))
	  (plane
	   (/ x-size 2) (/ y-size 2) (- (/ z-size 2))
	   (/ x-size 2) (- (/ y-size 2)) (- (/ z-size 2))
	   (- (/ x-size 2)) (/ y-size 2) (- (/ z-size 2))
	   (- (/ x-size 2)) (- (/ y-size 2)) (- (/ z-size 2)))
	  (plane
	   (- (/ x-size 2)) (/ y-size 2) (- (/ z-size 2))
	   (- (/ x-size 2)) (- (/ y-size 2)) (- (/ z-size 2))
	   (- (/ x-size 2)) (/ y-size 2) (/ z-size 2)
	   (- (/ x-size 2)) (- (/ y-size 2)) (/ z-size 2))
	  (plane
	   (/ x-size 2) (/ y-size 2) (/ z-size 2)
	   (/ x-size 2) (/ y-size 2) (- (/ z-size 2))
	   (- (/ x-size 2)) (/ y-size 2) (/ z-size 2)
	   (- (/ x-size 2)) (/ y-size 2) (- (/ z-size 2)))
	  (plane
	   (- (/ x-size 2)) (- (/ y-size 2)) (/ z-size 2)
	   (- (/ x-size 2)) (- (/ y-size 2)) (- (/ z-size 2))
	   (/ x-size 2) (- (/ y-size 2)) (/ z-size 2)
	   (/ x-size 2) (- (/ y-size 2)) (- (/ z-size 2)))))

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))

(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))

(defun update-entity (entity)
  (let ((m2w (reduce #'m4:* (list (m4:translation (pos entity))
				  (m4:rotation-from-euler (rot entity))
				  (m4:scale (scale entity))))))
    (setf (rot entity) (v:+ (rot entity) (v! 0.01 0.015 0.02)))
    (map-g #'prog-1 (e-stream entity))))

(defparameter *data* (box 1 1 1))

(defun init ()
  (let* ((verts (make-gpu-array *data*
				:element-type 'pos-col))
	 (e-stream (make-buffer-stream verts)))
    (setf *entities*
	  (mapcar (lambda (_) (make-entity :pos _ :e-stream e-stream))
		  (list (v! 0 0 -10))))))

(defun step-demo ()
  ;; (step-host)
  ;; (update-repl-link)
  ;; (clear)				
  (map nil #'update-entity *entities*)	
  (swap))
					   
(defun run-loop ()
  (init)
  (setf *running* t
	*array* (make-gpu-array *data*
				:element-type 'pos-col)
        *stream* (make-buffer-stream *array*))
  (loop :while (and *running* (not (shutting-down-p))) :do
     (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

;;;; ******************************END OF CEPL CODE********************************

(defclass 3bovr-test ()
  ((window :accessor win :initarg window)
   (hmd :reader hmd :initarg :hmd)
   (world-vao :accessor world-vao)
   (count :accessor world-count)
   (hud-vbo :accessor hud-vbo :initform nil)
   (hud-vao :accessor hud-vao :initform nil)
   (hud-count :accessor hud-count)
   (hud-texture :accessor hud-texture)
   (font :accessor font)))

(defparameter *tex-size* 256)

(defmethod glop:on-event ((window 3bovr-test) (event glop:key-event))
  ;; exit on ESC key
  (when (glop:pressed event)
    (case (glop:keysym event)
      (:escape
       (cepl.host::shutdown)))))

(defun hud-text (win hmd)
  (declare (ignorable win))
  (format nil "fps: ~s~%~
latency = ~{m2p:~,3,3f ren:~,3,3f tWrp:~,3,3f~%~
          PostPresent: ~,3,3f Err: ~,3,3f~}"
          "??"
          (%ovr::get-float-array
           hmd :dk2-latency 5)))

(defmethod glop:on-event ((window 3bovr-test) event)
  ;; ignore any other events
  (declare (ignore window event)))

(defun init-hud (win)
  (let ((vbo (gl:gen-buffer))
        (vao (hud-vao win)))
    (setf (hud-vbo win) vbo)
    (setf (hud-count win) 0)
    (let ((stride (* 4 4))) ;; x,y,u,v * float
      (gl:bind-buffer :array-buffer vbo)
      (%gl:buffer-data :array-buffer (* 0 stride) (cffi:null-pointer)
                       :static-draw)
      (gl:bind-vertex-array vao)
      (gl:enable-client-state :vertex-array)
      (%gl:vertex-pointer 2 :float stride (cffi:null-pointer))
      (gl:enable-client-state :texture-coord-array)
      (%gl:tex-coord-pointer 2 :float stride (* 2 4)))))

(defun update-hud (win string atl)
  (let* ((strings (split-sequence:split-sequence #\newline string))
         (count (reduce '+ strings :key 'length))
        (stride (* (+ 2 2) 6)) ;; x,y,u,v * 2 tris
        (i 0)
        (scale 0.01))
    (gl:bind-buffer :array-buffer (hud-vbo win))
    (%gl:buffer-data :array-buffer (* count stride 4) (cffi:null-pointer)
                     :static-draw)
    (let ((p (%gl:map-buffer :array-buffer :write-only)))
      (unwind-protect
           (loop for line in strings
                 for baseline from 0 by (* 30 scale)
                 when line
                   do (flet ((c (x y u v)
                               (let ((x (* x scale))
                                     (y (+ baseline (* y scale))))
                                 (setf (cffi:mem-aref p :float (+ 0 (* i 4))) x
                                       (cffi:mem-aref p :float (+ 1 (* i 4))) (- y)
                                       (cffi:mem-aref p :float (+ 2 (* i 4))) v
                                       (cffi:mem-aref p :float (+ 3 (* i 4))) u)
                                 (incf i))))
                        (texatl.cl:do-texatl-string (line
                                                     x0 y0 x1 y1
                                                     u0 v0 u1 v1
                                                     :tex-width *tex-size*
                                                     :tex-height *tex-size*)
                                                    atl
                          (c x0 y0 u0 v0)
                          (c x0 y1 u0 v1)
                          (c x1 y1 u1 v1)

                          (c x0 y0 u0 v0)
                          (c x1 y1 u1 v1)
                          (c x1 y0 u1 v0)))
                 finally (setf (hud-count win) i))
        (%gl:unmap-buffer :array-buffer)))))

(defparameter *w* nil)
(defun draw-world (win)
  (setf *w* win)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :framebuffer-srgb
             :line-smooth :blend :point-smooth :depth-test
             :lighting :light0 :color-material)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  (gl:light :light0 :position '(100.0 -120.0 -10.0 0.0))
  (when (world-count win)
    (gl:disable :texture-2d)
    ;; (gl:bind-vertex-array (world-vao win))
    ;; (%gl:draw-arrays :triangles 0 (world-count win)))
    (make-buffer-stream (make-gpu-array *data* :element-type 'pos-col))
  (gl:point-size 10)
  (gl:with-pushed-matrix* (:modelview)
    ;(gl:load-identity)
    (gl:translate -2 0.2 -2.5)
    (when (and (hud-count win) (plusp (hud-count win)))
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d (hud-texture win))
      (gl:bind-vertex-array (hud-vao win))
      (%gl:draw-arrays :triangles 0 (hud-count win))))
    (gl:bind-vertex-array 0)))



(defun draw-frame (hmd &key eye-render-desc fbo eye-textures win)
  (assert (and eye-render-desc fbo eye-textures))
  (let* ((timing (%ovrhmd::begin-frame hmd
                                       ;; don't need to pass index
                                       ;; unless we use
                                       ;; get-frame-timing
                                       0))
         ;;(props (%ovr::dump-hmd-to-plist hmd))
         ;; get current hmd position/orientation
         ;;(state (%ovrhmd::get-tracking-state hmd))
         ;;(pose (getf state :head-pose))
         ;;(pos (getf (getf pose :the-pose) :position))
         ;;(or (getf (getf pose :the-pose) :orientation))
         ;;(lac (getf pose :linear-acceleration))
         ;;(lv (getf pose :linear-velocity))
         ;;(cam (getf state :camera-pose))
         ;;(cam-pos (getf cam :position))
         ;; set camera orientation from rift
         #++(camera ))
    (declare (ignorable timing))
    ;; get position of eyes
    (multiple-value-bind (head-pose tracking-state)
        (%ovr::get-eye-poses hmd
                             (mapcar (lambda (a)
                                           (getf a :hmd-to-eye-view-offset))
                                         eye-render-desc))

      (let ((status (getf tracking-state :status-flags)))
        ;; change clear color depending on tracking state
        ;; red = no tracking
        ;; blue = orientation only
        ;; green = good
;        (print status)
        (cond
          ((and (member :orientation-tracked status)
                (member :position-tracked status))
           (gl:clear-color 0.1 0.5 0.2 1))
          ((and (member :orientation-tracked status))
           (gl:clear-color 0.1 0.2 0.5 1))
          (t
           (gl:clear-color 0.5 0.1 0.1 1))))
      ;; draw view from each eye
      ;; (gl:bind-framebuffer :framebuffer fbo) 
      (loop
        for index below 2
        ;; sdk specifies preferred drawing order, so it can predict
        ;; timing better in case one eye will be displayed before
        ;; other
        for eye = index ;(elt (getf props :eye-render-order) index)
        ;; get position/orientation for specified eye
        for pose = (elt head-pose eye)
        for orientation = (getf pose :orientation)
        for position = (getf pose :position)
        ;; get projection matrix from sdk
        for projection = (%ovr::matrix4f-projection
                          (getf (elt eye-render-desc eye)
                                :fov)
                          0.1 1000.0 ;; near/far
                          ;; request GL style matrix
                          '(:right-handed :clip-range-open-gl))
        ;; draw scene to fbo for 1 eye
        do (flet ((viewport (x)
                    ;; set viewport and scissor from texture config we
                    ;; will pass to sdk so rendering matches
                    (destructuring-bind (&key pos size) x
                      (gl:viewport (elt pos 0) (elt pos 1)
                                   (getf size :w) (getf size :h))
                      (gl:scissor (elt pos 0) (elt pos 1)
                                  (getf size :w)
                                  (getf size :h)))))
             (viewport (getf (elt eye-textures index) :render-viewport)))
      ;; pass textures to SDK for distortion, display and vsync
      (%ovr::end-frame hmd head-pose eye-textures)))))

(defparameter *once* nil)

(defun reset ()
  (setf *once* nil)
  (cepl::quit)
  (stop-loop))

(defun test-3bovr ()
  (when *once*
    ;; running it twice at once breaks things, so try to avoid that...
    (format t "already running?~%")
    (return-from test-3bovr nil))
  ;; initialize library
  (setf *once* t)
  (unwind-protect
       (%ovr::with-ovr ok (:debug nil :timeout-ms 500)
         (unless ok
           (format t "couldn't initialize libovr~%")
           (return-from test-3bovr nil))
         ;; print out some info
         (format t "version: ~s~%" (%ovr::get-version-string))
         (format t "time = ~,3f~%" (%ovr::get-time-in-seconds))
         (format t "detect: ~s HMDs available~%" (%ovrhmd::detect))
         ;; try to open an HMD
         (%ovr::with-hmd (hmd)
           (unless hmd
             (format t "couldn't open hmd 0~%")
             (format t "error = ~s~%"(%ovrhmd::get-last-error (cffi:null-pointer)))
             (return-from test-3bovr nil))
           ;; print out info about the HMD
           (let ((props (%ovr::dump-hmd-to-plist hmd)) ;; decode the HMD struct
                 w h x y)
             (format t "got hmd ~{~s ~s~^~%        ~}~%" props)
             (format t "enabled caps = ~s~%" (%ovrhmd::get-enabled-caps hmd))
             (%ovrhmd::set-enabled-caps hmd '(:low-persistence
                                              :dynamic-prediction))
             (format t "             -> ~s~%" (%ovrhmd::get-enabled-caps hmd))
             ;; turn on the tracking
             (%ovrhmd::configure-tracking hmd
                                          ;; desired tracking capabilities
                                          '(:orientation :mag-yaw-correction
                                            :position)
                                          ;; required tracking capabilities
                                          nil)
             ;; figure out where to put the window
             (setf w (getf (getf props :resolution) :w))
             (setf h (getf (getf props :resolution) :h))
             (setf x (aref (getf props :window-pos) 0))
             (setf y (aref (getf props :window-pos) 1))
             #+linux
             (when (eq (getf props :type) :dk2)
               (format t "overriding resolution from ~sx~s to ~sx~s~%"
                       w h 1920 1080)
               (setf w 1920 h 1080))
             ;; create window
             (format t "opening ~sx~s window at ~s,~s~%" w h x y)
	     (cepl::init w h "cepl-ovr" t)
	     (let ((win (make-instance '3bovr-test)))
	       (setf (slot-value win 'window) cepl.glop::*window*
		     (slot-value win 'hmd) hmd)
	     ;;   (setf (slot-value win 'win-class) '3bovr-test)
             ;; (glop:with-window (win
             ;;                    "cepl-ovr test window"
             ;;                    w h
             ;;                    :x x :y y
             ;;                    :win-class '3bovr-test
             ;;                    :fullscreen nil
	       ;;                    :depth-size 16)
	       ;;(inspect hmd) make my own 3bovr-test object with a window component
               ;; configure rendering and save eye render params
               ;; todo: linux/mac versions
               (%ovr::with-configure-rendering eye-render-desc
                   (hmd
                    ;; specify window size since defaults don't match on
                    ;; linux sdk with non-rotated dk2
                    :back-buffer-size (list :w w :h h)
                    ;; optional: specify which window/DC to draw into
                    ;;#+linux :linux-display
                    ;;#+linux(glop:x11-window-display win)
                    ;;#+windows :win-window
                    ;;#+windows(glop::win32-window-id win)
                    ;;#+windows :win-dc
                    ;;#+windows (glop::win32-window-dc win)
                    :distortion-caps
                    '(:time-warp :vignette
                      :srgb :overdrive :hq-distortion
                      #+linux :linux-dev-fullscreen))
                 ;; attach libovr runtime to window
                 #+windows
                 (%ovrhmd::attach-to-window hmd
                                            (glop::win32-window-id win)
                                            (cffi:null-pointer) (cffi:null-pointer))
                 ;; configure FBO for offscreen rendering of the eye views
                 (let* ((texture (make-texture nil :dimensions '(2 2) :element-type :uint8))
			(fbo (make-fbo (list 0 texture))) ;make an fbo with texture as an attachment			
                        ;; get recommended sizes of eye textures
                        (ls (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-left+
                                                           ;; use default fov
                                                           (getf (elt eye-render-desc
                                                                      %ovr::+eye-left+)
                                                                 :fov)
                                                           ;; and no scaling
                                                           1.0))
                        (rs (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-right+
                                                           (getf (elt eye-render-desc
                                                                      %ovr::+eye-right+)
                                                                 :fov)
                                                           1.0))
                        ;; put space between eyes to avoid interference
                        (padding 16)
                        ;; storing both eyes in 1 texture, so figure out combined size
                        (fbo-w (+ (getf ls :w) (getf rs :w) (* 3 padding)))
                        (fbo-h (+ (* 2 padding)
                                  (max (getf ls :h) (getf rs :h))))
                        ;; describe the texture configuration for libovr
                        (eye-textures
                          (loop for v in (list (list :pos (vector padding
                                                                  padding)
                                                     :size ls)
                                               (list :pos (vector
                                                           (+ (* 2 padding)
                                                              (getf ls :w))
                                                           padding)
                                                     :size rs))
                                collect
                                `(:texture ,texture
                                  :render-viewport ,v
                                  :texture-size (:w ,fbo-w :h ,fbo-h)
                                  :api :opengl))))
                   ;; configure the fbo/texture
                   (format t "left eye tex size = ~s, right = ~s~% total =~sx~a~%"
                           ls rs fbo-w fbo-h)

		   ;;What needs to happen here, is I need to make
		   ;;a texture out of the above code, then make an fbo and use that texture as an
		   ;;attachment (depth or color, I'm not sure).
		   ;;FBO attachments are slots for texture-backed gpu arrays. I think that
		   ;;texture-backed gpu-arrays are created by make-texture, and go along with the
		   ;;texture they were created with. So, we need to attach a texture to an fbo.
		   ;;The variable 'texture' is the texture we want to use.
		   ;;The fbo should have one color attachment, and maybe one depth attachment.
		   ;;The depth attachment seems to be related to the renderbuffer that 3b uses, but
		   ;;CEPL doesn't seem to reference a renderbuffer object at all. In the end though,
		   ;;what I need is a texture for each eye of the Rift, and something to render into
		   ;;them.
		   ;;A renderbuffer may be synonymous with a buffer-texture in CEPL.

		   ;; (sample texture) ;defaults will work, probably

		   (let ((sample (sample texture)))
		     (setf (wrap sample) :repeat
			   (minify-filter sample) :linear
			   (magnify-filter sample) :linear)
		   
		   (glop:set-gl-window (slot-value win 'window))

                   ;; main loop
		   (init)
		   (with-fbo-bound (fbo)
		     (setf *running* t)
		     (loop :while (and (glop:dispatch-events (slot-value win 'window)
							     :blocking nil
							     :on-foo nil)
				       *running*
				       (not (shutting-down-p)))
			:do (sample texture)
			:do (continuable (step-demo))))
		   (stop-loop)
		   (clear)
		   (setf *once* nil)
                   (format t "done~%")
                   (sleep 1))))))))))

#++
(asdf:load-systems '3b-ovr-sample)

#++
(test-3bovr)

#++
(let ((*default-pathname-defaults* (asdf:system-relative-pathname '3b-ovr "./")))
  (texatl:make-font-atlas-files "font.png" "font.met" 256 256
                                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                                16
                                :dpi 128
                                :padding 4
                                :string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[]= "))
