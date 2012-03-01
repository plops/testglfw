(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-glfw)
  (require :cl-glfw-opengl-version_1_1)
  (require :cl-glfw-glu))



(defun draw-grid ()
  (let ((g 110)
	(e 20))
    (gl:color-3ub g g g)
    (progn ;; coarse grid x
      (gl:line-width 1)
      (gl:with-begin gl:+lines+
	(loop for i from (- e) to e by 5 do 
	     (gl:vertex-3f (- e) i 0) 
	     (gl:vertex-3f e i 0))))
    (progn ;; fine grid x
      (gl:line-width .1) 
      (gl:with-begin gl:+lines+
	(loop for i from (- e) to e do
	     (gl:vertex-3f (- e) i 0) 
	     (gl:vertex-3f e i 0))))
    
    (progn ;; coarse grid y
      (gl:line-width 1)
      (gl:with-begin gl:+lines+
	(loop for i from (- e) to e by 5 do 
	     (gl:vertex-3f i (- e) 0) 
	     (gl:vertex-3f i e 0))))
    (progn ;; fine grid y
      (gl:line-width .1) 
      (gl:with-begin gl:+lines+
	(loop for i from (- e) to e do
	     (gl:vertex-3f i (- e) 0) 
	     (gl:vertex-3f i e 0)))))
   
   (gl:line-width 5)
   (gl:with-begin gl:+lines+
     (gl:color-3ub 150 80 30) (gl:vertex-3f 0 0 0) (gl:vertex-3f 5 0 0)
     (gl:color-3ub 100 150 30) (gl:vertex-3f 0 0 0) (gl:vertex-3f 0 5 0)
     (gl:color-3ub 20 100 155) (gl:vertex-3f 0 0 0) (gl:vertex-3f 0 0 5)))

(let ((t1 0d0)
      (t0 0d0)
      (frames 0))
 (defun count-fps ()
   (setf t1 (glfw:get-time))
   (when (or (< 1 (- t1 t0))
	     (= frames 0))
     (glfw:set-window-title (format nil "bla ~,1f FPS"
				    (/ frames (- t1 t0))))
     (setf frames 0
	   t0 t1))
   (incf frames)))

(defun draw-tetraeder ()
  (gl:color-3ub 255 255 255)
  (gl:line-width 1)
  (gl:with-begin gl:+line-loop+
    (loop for (x y z) in '((0 0 1) (0 1 0) (1 0 0)
			   (1 1 1) (0 0 1) (0 1 0)) do
	 (gl:vertex-3f x y z))))

(let ((rot 0))
  (defun draw ()
    (destructuring-bind (w h) (glfw:get-window-size)
      (setf h (max h 1))
      (gl:viewport 0 0 w h)
      (gl:clear-color .0 .2 .2 1)
      (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (gl:matrix-mode gl:+projection+)
      (gl:load-identity)
      (glu:perspective 65 (/ w h) 1 100)
      (gl:matrix-mode gl:+modelview+)
      (gl:load-identity)
      (glu:look-at 0 20 14 ;; camera
		   0 0 0   ;; target
		   0 0 1))
    (gl:translate-f 0 0 0)
    (gl:rotate-f 30 0 0 1)
    (if (< rot 360)
	(incf rot .3)
	(setf rot 0))
    (draw-grid)
    (count-fps)
    (gl:line-width 3)
    (gl:color-3f 1 1 1)
    (gl:enable gl:+lighting+)
    (gl:enable gl:+depth-test+)
    (gl:enable gl:+light0+)
    (gl:shade-model gl:+flat+)
    (gl:light-fv gl:+light0+ gl:+position+ #(1s0 4s0 2s0 1s0))
    (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ #(0.9 0.9 0.0 1.0))

    ;;(gl:disable gl:+normalize+)
    
    (gl:with-push-matrix
      (gl:rotate-f rot 0 0 1)
      (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ 
		      (make-array 4 :initial-element 1s0 
				  :element-type 'single-float))
      (let* ((s 1)
	     (h 16)
	     (w 16)
	     (ww 32)
	     (hh 32)
	     (img (make-array (list hh ww) :element-type '(unsigned-byte 16)))
	     (objs (make-array 1 :element-type '(unsigned-byte 32))))
	(gl:gen-textures (length objs) objs)
	(gl:bind-texture gl:+texture-2d+ (aref objs 0))
	;;(gl:pixel-store-i gl:+unpack-alignment+ 1)
	(gl:tex-parameter-i gl:+texture-2d+ 
			    gl:+texture-min-filter+ gl:+nearest+)
	(gl:tex-parameter-i gl:+texture-2d+ 
			    gl:+texture-mag-filter+ gl:+nearest+)
	(gl:enable gl:+texture-2d+)
	
	(dotimes (i ww)
	  (dotimes (j hh)
	    (setf (aref img j i) (* (/ (expt 2 16) 32) j (mod (* i j) 2)))))
	;; (defparameter *rend* (gl:get-string gl:+renderer+))
	(gl:matrix-mode gl:+color-matrix-sgi+
	 )
	; (gl:load-identity)
	(gl:matrix-mode gl:+modelview+)

	(sb-sys:with-pinned-objects (img)
	  (gl:tex-image-2d gl:+texture-2d+ 0 gl:+luminance+ ww hh 0
			   gl:+luminance+ gl:+unsigned-short+
			   (sb-sys:vector-sap 
			    (sb-ext:array-storage-vector img))))
	
	(let ((a (gl:get-error)))
	  (unless (= a 0)
	    (format t "get-error: ~a~%" a)))
	(gl:scale-f s s s)
	(gl:translate-f (* w -.5) (* h -.5) .1)
	
	#+nil
	(progn
	 (gl-ext:scan-available-extensions)
	 (defparameter *bla* (gl-ext:available-extensions))
	 (defparameter *bla2* (gl:get-string gl:+extensions+)))

	(gl:with-begin gl:+quads+
	  (dotimes (j h)
	    (let ((d -.02
		    ))
	      (dotimes (i w)
		(labels ((c (a b)
			   (gl:tex-coord-2f (* a (/ 1s0 w)) 
					    (* b (/ 1s0 h)))
			   (gl:vertex-2f a b)))
		  (gl:normal-3f 0 0 s)
		  (c i j)
		  (c i (+ d 1 j))
		  (c (+ d 1 i) (+ d 1 j))
		  (c (+ d 1 i) j))))))
	(gl:disable gl:+lighting+)
	(gl:disable gl:+texture-2d+)
	(gl:delete-textures 1 objs)))))

(glfw:do-window (:title "bla" :width 512 :height 512)
    ()
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))

