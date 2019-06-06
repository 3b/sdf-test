(asdf:defsystem :sdf-test
  :description "test/viewer/comparison app for sdf fonts"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (sdf cl-glut cl-glu cl-opengl 3bgl-shader sb-cga
                   zpb-ttf cl-vectors cl-aa cl-paths-ttf)
  :serial t
  :components ((:file "package")
               (:file "shaders")
               (:file "viewer")))
