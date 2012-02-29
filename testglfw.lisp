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
    (gl:clear gl:+color-buffer-bit+)
    (gl:matrix-mode gl:+projection+)
    (gl:load-identity)
    (glu:perspective 65 (/ w h) 1 100)
    (gl:matrix-mode gl:+modelview+)
    (gl:load-identity)
    (glu:look-at 0 20 14 ;; camera
		 0 0 0 ;; target
		 0 0 1))
   (gl:translate-f 0 0 0)
   (gl:rotate-f rot 0 0 1)
   (if (< rot 360)
       (incf rot .3)
       (setf rot 0))
   (draw-grid)
   (count-fps)
   (gl:line-width 1)
   (gl:color-3f 1 1 1)
   (let ((s 10))
    (gl:scale-d s s s))
   (gl:with-begin gl:+line-loop+
     (gl:vertex-2f 0 0)
     (gl:vertex-2f 0 1)
     (gl:vertex-2f 1 1)
     (gl:vertex-2f 1 0)
     )
   ))

(glfw:do-window (:title "bla" :width 512 :height 512)
    ()
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))