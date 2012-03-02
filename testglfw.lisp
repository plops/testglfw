(eval-when (:compile-toplevel :execute :load-toplevel)
  (load "c:/Users/martin/quicklisp/setup.lisp")
  (require :cl-glfw)
  (require :cl-glfw-opengl-version_1_1)
  (require :cl-glfw-glu))



(defun draw-grid ()
  (gl:disable gl:+lighting+)
  (gl:disable gl:+texture-2d+)
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
     (gl:color-3ub 20 100 155) (gl:vertex-3f 0 0 0) (gl:vertex-3f 0 0 5))
   (gl:color-3ub 255 255 255)
   (gl:enable gl:+lighting+)
   (gl:enable gl:+texture-2d+))

(let ((t1 0d0)
      (t0 0d0)
      (frames 0))
  (defun reset-fps-counter ()
    (setf frames 0))
  (defun count-fps ()
    (setf t1 (glfw:get-time))
    (when (or (< 1 (- t1 t0))
	      (= frames 0))
      (glfw:set-window-title (format nil "bla ~,1f FPS"
				     (if (< (- t1 t0) 1e-6)
					 0s0
					 (/ frames (- t1 t0)))))
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

(defun init-gl-state ()
  (gl:enable gl:+lighting+)
  (gl:enable gl:+depth-test+)
  (gl:enable gl:+light0+)
  (gl:shade-model gl:+flat+)
  (gl:light-fv gl:+light0+ gl:+position+ #(1s0 4s0 2s0 1s0))
  (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ #(0.9 0.9 0.0 1.0))
  (gl:enable gl:+normalize+))

(let ((objs nil))
 (defun upload-texture (h w)
   (let* ((img (make-array (list h w) :element-type '(unsigned-byte 16))))
     (unless objs
       (setf objs (make-array 1 :element-type '(unsigned-byte 32)))
       (gl:gen-textures (length objs) objs))
     
     (gl:delete-textures 1 objs)
     (gl:gen-textures (length objs) objs)
     (gl:bind-texture gl:+texture-2d+ (aref objs 0))
     ;;(gl:pixel-store-i gl:+unpack-alignment+ 1)
     (gl:tex-parameter-i gl:+texture-2d+ 
			 gl:+texture-min-filter+ gl:+nearest+)
     (gl:tex-parameter-i gl:+texture-2d+ 
			 gl:+texture-mag-filter+ gl:+nearest+)
     (gl:enable gl:+texture-2d+)
	  
     (dotimes (i w)
       (dotimes (j h)
	 (setf (aref img j i) (* (/ (expt 2 8) 32) j (mod (* i j) 2)))))
    
     (gl:matrix-mode gl:+color+)
     (gl:load-identity)
     (gl:scale-f 2253 .1 .1)
     (gl:matrix-mode gl:+modelview+)
    
     (sb-sys:with-pinned-objects (img)
       (gl:tex-image-2d gl:+texture-2d+ 0 gl:+luminance+ w h 0
			gl:+luminance+ gl:+unsigned-short+
			(sb-sys:vector-sap 
			 (sb-ext:array-storage-vector img))))
    
     (let ((a (gl:get-error)))
       (unless (= a 0)
	 (format t "get-error: ~a~%" a))))))

;; store the position of the quad in a 32bit number
(defun encode-pick-name (x y)
  (logior (ash x 16)
	  y))

(defun decode-pick-name-x (p)
  (ldb (byte 16 16) p))

(defun decode-pick-name-y (p)
  (ldb (byte 16 0) p))

(defun draw-quads (&key select w h x y)
  (unless select 
    (gl:begin gl:+quads+))
  (dotimes (j h)
    (let ((d  -.1s0
	    ))
      (dotimes (i w)
	(when select
	  (gl:load-name (encode-pick-name i j)))
	#+nil(if (and x y (= x i) (= y j))
	  (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ 
			  (make-array 4 :initial-contents '(.3s0 1s0 .4s0 1s0) 
				      :element-type 'single-float))
	  (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ 
			  (make-array 4 :initial-contents '(1s0 1s0 1s0 1s0) 
				      :element-type 'single-float)))
	(progn
	  (when select
	    (gl:begin gl:+quads+))
	  (labels ((c (a b)
		     (unless select
		       (gl:tex-coord-2f (* a (/ 1s0 w)) 
					(* b (/ 1s0 h))))
		     (gl:vertex-2f a b)))
	    (gl:normal-3f 0 0 1)
	    (c i j)
	    (c i (+ d 1 j))
	    (c (+ d 1 i) (+ d 1 j))
	    (c (+ d 1 i) j))
	  (when select
	    (gl:end))))))
  (unless select
    (gl:end)))

(defun set-view3d (&key select)
  (destructuring-bind (w h) (glfw:get-window-size)
    (setf h (max h 1))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode gl:+projection+)
    (gl:load-identity)
    
    (destructuring-bind (x y) (glfw:get-mouse-pos)
      (when select
	(let ((v (make-array 4 :element-type '(signed-byte 32)
			     :initial-contents (list 0 0 w h))))
	  (sb-sys:with-pinned-objects (v)
	   (glu:pick-matrix x (- h y) 1 1 (sb-sys:vector-sap v)))))
      (glu:perspective 65 (/ w h) 1 100))
       
    (gl:matrix-mode gl:+modelview+)
    (gl:load-identity)
    
    (glu:look-at 10 20 14 ;; camera
		 0 0 0 ;; target
		 0 0 1)))

(let ((rot 0)
      (set-int nil))
  (defun draw ()
    (gl:clear-color .0 .2 .2 1)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    
    (unless set-int
      (glfw:swap-interval 1)
      (setf set-int t))
    
    
  
    (init-gl-state)
    (draw-grid)
    
    (incf rot 1)
    (if (< 360 rot)
	(setf rot 0))
    
    (count-fps)


    (let* ((s 1)
	   (h 32)
	   (w 32)
	   (ww 32)
	   (hh 32)) 
      (let ((sx 0)
	    (sy 0))
      #+nil	(gl:with-push-matrix
	  (set-view3d :select t)
	  (gl:rotate-f rot 0 0 1)
	  (gl:scale-f s s s)
	  (gl:translate-f (* w -.5) (* h -.5) .1)
	  (let* ((n (* w h))
		 (sel (make-array (*  ; n, min-depth, max-depth, name
				   4 n)
				  :element-type '(unsigned-byte 32))))
	    (sb-sys:with-pinned-objects (sel)
	      (gl:select-buffer (length sel) (sb-sys:vector-sap sel))
	      (gl:render-mode gl:+select+)
	      (gl:init-names)
	      (gl:push-name 11111111) ;; name stack must contain one value
	      (draw-quads :select t :w w :h h)
	      (gl:pop-name)
	      (let ((sn (gl:render-mode gl:+render+)))
		(when (< sn 0)
		  (break "problem with select. maybe too many objects"))
		(when (< 0 sn)
		  (setf sx (decode-pick-name-x (aref sel 3))
			sy (decode-pick-name-y (aref sel 3))))))))
       
       
      ;; (upload-texture hh ww)
       (set-view3d :select nil)
       (gl:with-push-matrix 
	 (gl:rotate-f rot 0 0 1)
	 (gl:scale-f s s s)
	 (gl:translate-f (* w -.5) (* h -.5) .1)
	 (draw-quads :select nil :w w :h h :x sx :y sy)))
      
      )))

(progn
  (reset-fps-counter)
 (glfw:do-window (:title "bla" :width 512 :height 512)
     ()
   (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
     (return-from glfw::do-open-window))
   (draw)))

