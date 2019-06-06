(in-package sdf-test)

(defclass sdf-test (glut:window)
  ((chr :accessor chr :initform #\A)
   (ref-tex :accessor ref-tex :initform nil)
   (tex :accessor tex :initform nil)
   (program :accessor program
            ;; gl name, modified flags per stage, names per stage
            :initform (list nil (list t t)
                            (list 'sdf-test-shaders:vertex
                                  'sdf-test-shaders:fragment))))
  (:default-initargs :width 1024 :height 1024 :title "sdf test"
                     :mode '(:double :rgb :depth)
                     :right-menu '(:click :exit)))

(defparameter *modified-shader-functions* nil)

(defun modified-shader-hook (modified)
  (format t "saw modified functions ~s~%" modified)
  (setf *modified-shader-functions*
        (union modified *modified-shader-functions*)))

(pushnew 'modified-shader-hook 3bgl-shaders::*modified-function-hook*)

(setf 3bgl-shaders::*print-shaders* t)
(defun recompile-modified-shaders (w)
  (let* ((m *modified-shader-functions*)
         (current-program (program w)))
    ;; flag any shaders we are using
    (loop for (p f names) in (list (program w))
          do (loop for n in names
                   for i from 0
                   when (member n m)
                     do (setf (nth i f) t)))
    ;; fixme: this needs a lock, since it could be modified from
    ;; another thread
    (setf *modified-shader-functions* nil)

    (destructuring-bind (&optional program flags names)
        current-program
      (when (and names
                 (or (and program (some #'identity flags))
                     (and (not program) (every #'identity flags))))
        (format t "~%recompiling shader program for changes in functions:~&  ~a~%"
                (loop for f in flags for n in names when f collect n))
        (setf (second current-program) (substitute nil t flags))
        (time
         (setf (car current-program)
               (3bgl-shaders::reload-program (first current-program)
                                             (first (third current-program))
                                             (second (third current-program))
                                             :geometry (third (third current-program))
                                             :version 330)))))))


(defun solid-cube ()
  (let ((v #((-1 -1 1) (1 -1 1) (1 1 1) (-1 1 1)
             (-1 -1 -1) (1 -1 -1) (1 1 -1) (-1 1 -1))))
    (flet ((q (a b c d)
             (gl:tex-coord 0 0)
             (apply 'gl:vertex (aref v a))
             (gl:tex-coord 1 0)
             (apply 'gl:vertex (aref v b))
             (gl:tex-coord 1 1)
             (apply 'gl:vertex (aref v c))
             (gl:tex-coord 0 1)
             (apply 'gl:vertex (aref v d))))
      (gl:with-primitives :quads
        (gl:normal 0 0 1)
        (q 0 1 2 3)
        (gl:normal 0 0 -1)
        (q 5 6 7 4)
        (gl:normal -1 0 0)
        (q 4 0 3 7)
        (gl:normal 1 0 0)
        (q 1 2 6 5)
        (gl:normal 0 1 0)
        (q 3 2 6 7)
        (gl:normal 0 -1 0)
        (q 4 0 1 5)))))

(defmethod glut:display-window :before ((w sdf-test))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test))

(declaim (inline deg-to-rad rad-to-deg))
(defun deg-to-rad (x)
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.9))
    (t (* x (/ pi 180)))))
(defun rad-to-deg (x)
  (typecase x
    (single-float
     (float (* x (/ 180.0 pi)) 1.0))
    (t (* x (/ 180 pi)))))

(defun perspective-matrix (fovy-degrees aspect z-near z-far)
  (let ((f (float (/ (tan (/ (deg-to-rad fovy-degrees) 2))) 1.0))
        (dz (- z-near z-far)))
    (sb-cga:matrix (/ f aspect) 0.0 0.0 0.0
                   0.0 f 0.0 0.0
                   0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
                   0.0 0.0 -1.0 0.0)))

(defvar *r* 0)
(defmethod glut:display ((window sdf-test))
  (recompile-modified-shaders window)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear :color-buffer :depth-buffer)
  (when (tex window)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (tex window)))
  (when (ref-tex window)
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (ref-tex window)))
  (flet ((radians (x) (coerce (/ (* pi x) 180) 'single-float))
         (v (x y z) (sb-cga:vec (float x 1.0)
                                (float y 1.0)
                                (float z 1.0))))
    (let* ((m1 (sb-cga:matrix*
                (sb-cga:scale* 0.5 0.5 0.5)
                (sb-cga:translate (v 0 0.0 0))
                (sb-cga:rotate-around (v 0 1 0)
                                      (radians *r*))
                (sb-cga:rotate-around (v 1 0 0) (radians -90))))
           (m2 (sb-cga:matrix*
                (sb-cga:scale* 1.0 1.0 1.0)
                (sb-cga:translate (v 0 -1 0))))
           (v (sb-cga:matrix*
               (sb-cga:translate (v 0 1 -3.1))
               #++(sb-cga:rotate-around (v 1 1 0)
                                        (radians 30))))
           (p (perspective-matrix 50
                                  (/ (glut:width window)
                                     (glut:height window))
                                  0.5 20))
           (mv1 (sb-cga:matrix* v m1))
           (mv2 (sb-cga:matrix* v m2))
           (mvp1 (sb-cga:matrix* p v m1))
           (mvp2 (sb-cga:matrix* p v m2))
           (p1 (car (program window))))
      (gl:clear-color (random 0.04) 0 0 1)

      (when p1
        (gl:use-program p1)
        (3bgl-shaders::uniformi p1 "tex0" 0)
        (3bgl-shaders::uniformi p1 "tex1" 1)
        (3bgl-shaders::uniformi p1 "flip" 1)
        (3bgl-shaders::uniform-matrix p1 "mv" mv1)
        (3bgl-shaders::uniform-matrix p1 "mvp" mvp1)
        (3bgl-shaders::uniform-matrix p1 "normalMatrix" mv1))
      (gl:front-face :cw)
      (glut:solid-teapot 1)
      (when p1
        (3bgl-shaders::uniformi p1 "flip" 0)
        (3bgl-shaders::uniform-matrix p1 "mv" mv2)
        (3bgl-shaders::uniform-matrix p1 "mvp" mvp2)
        (3bgl-shaders::uniform-matrix p1 "normalMatrix" mv2))
      (gl:front-face :ccw)
      (solid-cube)))
  (gl:use-program 0)
  (glut:swap-buffers))

(defmethod glut:reshape ((window sdf-test) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 50 (/ width height) 0.5 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun flatten (image)
  (assert (= (array-dimension image 2) 3))
  (let ((dh (array-dimension image 0))
        (dw (array-dimension image 1))
        (f (make-array (array-total-size image))))
    (loop for i below (array-total-size image)
          for c = (mod i 3)
          for x = (mod (floor i 3) dw)
          for y = (floor i (* dw 3))
          do (setf (aref f i)
                   (aref image y x c)))
    (list dw dh f)))

(defun render-character-ref (font glyph)
  (let* ((gw (- (zpb-ttf:xmax glyph) (zpb-ttf:xmin glyph)))
         (gh (- (zpb-ttf:ymax glyph) (zpb-ttf:ymin glyph)))
         (scale (/ 1024 (zpb-ttf:units/em font)))
         (dw (ceiling (* scale gw)))
         (dh (ceiling (* scale gh))))
    (let* ((image (aa-misc:make-image dw dh #(0 0 0)))
           (state (aa:make-state))
           (px (aa-misc:image-put-pixel image #(255 255 255))))
      (vectors:update-state
       state (paths-ttf:paths-from-glyph
              glyph
              :offset (paths:make-point (- (/ (- dw (* scale gw)) 2)
                                           (* (zpb-ttf:xmin glyph) scale))
                                        (- (/ (- dh (* scale gh)) 2)
                                           (* (zpb-ttf:ymin glyph) scale)))
              :scale-x scale
              :scale-y scale))
      (aa:cells-sweep state (lambda (x y a)
                              (if (>= (abs a) 128)
                                  (funcall px x y 255))))
      (flatten image))))

(defparameter *font*
  (nth 2 '("c:/Windows/Fonts/yugothic.ttf"
           "c:/Windows/Fonts/simfang.ttf"
           "georgia.ttf"
           "c:/Windows/Fonts/arialbd.ttf"
           "c:/Windows/Fonts/arial.ttf")))
(defmethod (setf chr) :after (new (window sdf-test))
  (unless (tex window)
    (setf (tex window) (gl:gen-texture))
    (gl:bind-texture :texture-2d (tex window))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge))
  (unless (ref-tex window)
    (setf (ref-tex window) (gl:gen-texture))
    (gl:bind-texture :texture-2d (ref-tex window))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border))
  (gl:bind-texture :texture-2d (ref-tex window))
  (gl:pixel-store :unpack-alignment 1)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border)
  (zpb-ttf:with-font-loader (ttf *font*)
    (let* ((g (zpb-ttf:find-glyph new ttf))
           (a (when g (render-character-ref ttf g))))
      (if a
          (gl:tex-image-2d :texture-2d 0 :rgb (first a) (second a)
                           0 :rgb :unsigned-byte
                           (third a))
          (gl:tex-image-2d :texture-2d 0 :rgb 4 4 0 :rgb :unsigned-byte
                           #(255 0 0  255 0 0  0 0 0  0 0 0
                             255 0 0  255 0 0  0 0 0  0 0 0
                             0 0 0  0 0 0  0 0 255  0 255 0
                             0 0 0  0 0 0  0 255 0  0 0 255)))))
  (gl:bind-texture :texture-2d (tex window))
  (gl:pixel-store :unpack-alignment 1)
  (zpb-ttf:with-font-loader (ttf *font*)
    (let* ((g (zpb-ttf:find-glyph new ttf))
           (s 64)
           (a (when g
                (time
                 (flatten
                  #++
                  (sdf::sdf/ms ttf g
                               (float (/ s (- (zpb-ttf:ascender ttf)
                                              (zpb-ttf:descender ttf))))
                               16
                               0.35)
                  (sdf::msdf ttf g
                             (float (/ s (- (zpb-ttf:ascender ttf)
                                            (zpb-ttf:descender ttf))))
                             nil
                             0.4))))))
      (format t "~sx~s~%" (first a) (second a))
      (if a
          (gl:tex-image-2d :texture-2d 0 :rgb8 (first a) (second a)
                           0 :rgb :unsigned-byte
                           (third a))
          (gl:tex-image-2d :texture-2d 0 :rgb 4 4 0 :rgb :unsigned-byte
                           #(255 0 0  255 0 0  0 0 0  99 99 99
                             255 0 0  255 0 0  99 99 99 0 0 0
                             0 0 0  99 99 99 0 0 255  0 255 0
                             99 99 99  0 0 0  0 255 0  0 0 255))))))

(defmethod glut::menu ((window sdf-test) (menu (eql :character)) id)
  (format t "~&got menu item ~s from menu ~s~%" menu id)
  (setf (chr window) (char (symbol-name id) 0)))

(defmethod glut::menu ((window sdf-test) menu id)
  (format t "~&got menu item ~s from menu ~s~%" menu id)
  (when (and (eql menu :left-button)
             (= 1 (length (symbol-name id))))
    (setf (chr window) (char (symbol-name id) 0)))
  (case id
    (:click
     (setf (glut::right-menu window)
           `(:caps :lower :symbol :cjk :click :exit)))
    (:caps
     (setf (glut::left-menu window)
           (loop for i across "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 collect (list (string i)
                               (intern (string i)
                                       (find-package :keyword))))))
    (:lower
     (setf (glut::left-menu window)
           (loop for i across "abcdefghijklmnopqrstuvwxyz"
                 collect (list (string i)
                               (intern (string i)
                                       (find-package :keyword))))))
    (:symbol
     (setf (glut::left-menu window)
           (loop for i across "~`!@#$%^&*()_+=-|}{[]\":;'?><,./"
                 collect (list (string i)
                               (intern (string i)
                                       (find-package :keyword))))))
    (:cjk
     (setf (glut::left-menu window)
           (loop for i across "語"
                 collect (list (string i)
                               (intern (string i)
                                       (find-package :keyword))))))

    (:exit
     (glut:destroy-current-window))))

(defmethod glut:keyboard ((window sdf-test) key x y)
  (declare (ignore x y))
  (case key
    (#\space
     (setf (chr window) (chr window)))
    ( #\Esc
     (glut:destroy-current-window))))


(defvar *w* nil)
(defmethod glut:idle ((window sdf-test))
  (unless *w*
    (setf *w* window)
    (format t "foo!~%"))
  (glut:post-redisplay))

(defun sdf-test (&rest args)
  (glut:display-window (apply #'make-instance 'sdf-test args)))

#++
(ql:quickload 'sdf-test)
#++
(sdf-test :pos-x 2555)
#++
(glut:show-window)
#++
(glut:main-loop)
