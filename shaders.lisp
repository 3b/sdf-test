(defpackage #:sdf-test-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex
           #:fragment))
(in-package #:sdf-test-shaders)

(input position :vec4 :location 0)
(input (normal "gl_Normal") :vec3)
(input (uv "gl_MultiTexCoord0") :vec4)

(output color :vec4 :stage :fragment)


;; uniforms
(uniform mv :mat4) ;; model-view matrix
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform normal-matrix :mat4)
(uniform tex0 :sampler-2d)
(uniform tex1 :sampler-2d)
(uniform flip :bool)


;; output from vertex shader, interpolated by GL then sent as input to
;; fragment shader
;; visible in vertex shader as 'outs', and in fragment shader as 'ins'
(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec4)
  (normal :vec3)
  (color :vec4)
  (uv :vec4)
  (eye-direction :vec3)
  (light-direction :vec3))

;; some constants for lighting (would probably be uniforms in a real program)
(defconstant +ambient-color+ (vec3 0.0 0.024 0.06) :vec3)
(defconstant +diffuse-color+ (vec3 0.6 0.4 0.2)#++(vec3 0.2 0.4 0.6) :vec3)
(defconstant +specular-exponent+ 16 :float)
(defconstant +light-position+ (vec3 4 4 -5) :vec3)

;; generic vertex shader used for a few lighting models
(defun vertex ()
  (setf gl-position (* mvp position))
  (setf (@ outs normal) (* (mat3 normal-matrix) normal)
        (@ outs position) (* mv position)
        (@ outs uv) uv
        ;; interpolated lighting parameters
        (@ outs light-direction) (- +light-position+ (vec3 (* mv position)))
        (@ outs eye-direction) (- (vec3 (* mv position)))))

(defun median (v)
  (let ((a (.x v))
        (b (.y v))
        (c (.z v)))
    (return (max (min a b) (min (max a b) c)))))

(defun fragment ()
  ;; normalize the interpolated normal, since interpolation doesn't
  ;; preserve length
  (let* ((normal (normalize (@ ins normal)))
         ;; same for eye direction and light direction
         (eye-direction (normalize (@ ins eye-direction)))
         (light-direction (normalize (@ ins light-direction)))
         ;; calculate some intermediate values
         (l-dot-n (clamp (dot light-direction normal) 0 1))
         (r (reflect (- light-direction) normal))
         (r-dot-v (clamp (dot r eye-direction) 0 1))
         (distance (length (@ ins eye-direction)))
         (uv (.xy (@ ins uv)))
         (t0 (vec4 0 0 0 1))
         (t1 (vec4 0 0 0 1))
         (c 0.0)
         (b 0.0))
    (when flip
      (setf (.y uv) (- 0.95 (.y uv)))
      (setf (.x uv) (- 1 (.x uv))))
    (setf t0 (texture tex0 uv))
    (setf t1 (texture tex1 (+ 0.5 (* 1.2 (- uv 0.5)))))
    (let ((d (- (median (.xyz t0))
                0.5)))

      #++(setf c (smooth-step -0.01 0.01 d))
      (setf c (clamp (+ 0.5 (/ d (fwidth d))) 0 1))
      #++(setf b (clamp (+ 0.0 (/ (+ d 0.1) (fwidth d))) 0 1)))
    (setf color (vec4 (.y t1) c c 1))
    #++(setf color (vec4 (* 120 (- (.xyz t0) 0.5)) 1))
    #++(if (> b c)
           (setf color (vec4 0.4 0.4 0.4 1))
           (setf color (vec4 c c c 1)
                 #++(vec4 (+ +ambient-color+
                             (* (/ 2 distance)
                                (+ (* (vec3 (.xy (@ ins uv)) 0)
                                      +diffuse-color+
                                      l-dot-n)
                                   (pow r-dot-v +specular-exponent+))))
                          1)))))
