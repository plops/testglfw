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
  (gl:light-fv gl:+light0+ gl:+position+ #(1s0 3s0 5s0 1s0))
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
     (gl:scale-f 253 .1 .1)
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

(let ((ow 8)
      (oh 8)
      norms verts texco)
 (defun draw-quads (&key select (w 8) (h 8) emph)
   "if emph is a list of (x y) emphasize these"
   (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ 
		   (make-array 4 :initial-contents '(.8s0 .8s0 .8s0 1s0) 
			       :element-type 'single-float))
  
   (when (or ;; if no arrays are defined or the size changed
	  (/= w ow)
	  (/= h oh)
	  (not (and norms verts texco)))
     (setf ow w
	   oh h
	   norms (make-array (list h w 4 3) :element-type 'single-float
			     :initial-element 0s0)
	   verts (make-array (list h w 4 2) :element-type 'single-float)
	   texco (make-array (list h w 4 2) :element-type 'single-float))
     (dotimes (j h)
       (let ((d (/ -.2s0 w)))
	 (dotimes (i w)
	   (labels ((c (v a b)
		      (setf (aref verts j i v 0) a
			    (aref verts j i v 1) b
			    (aref texco j i v 0) (* a (/ 1s0 w))
			    (aref texco j i v 1) (* b (/ 1s0 h)))))
	     (dotimes (k 4)
	       (setf (aref norms j i k 0) 0s0
		     (aref norms j i k 1) 0s0
		     (aref norms j i k 2) 1s0))	 
	     (c 0 (* 1s0 i) (* 1s0 j))
	     (c 1 (* 1s0 i) (+ d 1 j))
	     (c 2 (+ d 1 i) (+ d 1 j))
	     (c 3 (+ d 1 i) (* 1s0 j)))))))

   (sb-sys:with-pinned-objects (texco norms verts)
       (gl:tex-coord-pointer 2
			     gl:+float+
			     0
			     (sb-sys:vector-sap 
			      (sb-ext:array-storage-vector 
			       texco)))
      (gl:normal-pointer gl:+float+
			 0
			 (sb-sys:vector-sap 
			  (sb-ext:array-storage-vector norms)))
       (gl:vertex-pointer 2
			  gl:+float+
			  0
			  (sb-sys:vector-sap 
			   (sb-ext:array-storage-vector verts)))
       (gl:enable-client-state gl:+vertex-array+)
       (if select
	   (progn (gl:disable-client-state gl:+normal-array+)
		  (gl:disable-client-state gl:+texture-coord-array+)
		  (dotimes (j h)
		    (dotimes (i w)
		      (gl:load-name (encode-pick-name i j))
		      (gl:draw-arrays gl:+quads+ 
				      (* 4 (+ i (* w j))) 4))))
	   (progn
	     (gl:enable-client-state gl:+normal-array+)
	     (gl:enable-client-state gl:+texture-coord-array+)
	     (gl:draw-arrays gl:+quads+ 0 (* w h 4))))

       (when (and (not select)
		  emph
		  (car emph)) ;; draw selection indicator
	 (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ 
			 (make-array 
			  4 :initial-contents '(.2s0 .8s0 .2s0 1s0) 
			  :element-type 'single-float))
	 (gl:with-push-matrix
	   (gl:translate-f 0 0 .1)
	   (dolist (e emph)
	     (destructuring-bind (x y) e
	       (gl:enable-client-state gl:+normal-array+)
	       (gl:enable-client-state gl:+texture-coord-array+)
	       (gl:draw-arrays gl:+quads+ (* 4 (+ x (* w y))) 4))))))))

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

(defparameter *current-quad* nil)
(defparameter *emph-quad-list* nil)

(defun mouse-button-callback (button action)
  (when (= action 0)
    (push *current-quad* *emph-quad-list*))
  (format t "~a~%" (list button (eq button 'left) action *current-quad*)))

(let ((rot 0)
      (set-int nil)
      (mouse-cb nil))
  (defun draw ()
    (gl:clear-color .0 .2 .2 1)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    
    (unless set-int
      (glfw:swap-interval 1)
      (setf set-int t))
    (sleep (/ .9 60))
    (unless mouse-cb
      (glfw:set-mouse-button-callback 'mouse-button-callback))
  
    
    (incf rot .1)
    (if (< 360 rot)
	(setf rot 0))
    
    (count-fps)


    (let* ((s 4)
	   (w 8)
	   (h 6)
	   (ww 320)
	   (hh 240)) 
      (let ((mpos nil))
      	(gl:with-push-matrix
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
		  (setf mpos (list (decode-pick-name-x (aref sel 3))
				   (decode-pick-name-y (aref sel 3))))))))
	  
	  (when mpos
	    (setf *current-quad* mpos))
	  
	  (upload-texture hh ww)
	  (set-view3d :select nil)
	  
	  (init-gl-state)
	  (draw-grid)
	  (gl:rotate-f rot 0 0 1)
	  (gl:scale-f s s s)
	  (gl:translate-f (* w -.5) (* h -.5) .1)

	  (draw-quads :select nil :w w :h h 
		      :emph 
		      (if mpos
			  (append (list mpos) *emph-quad-list*)
			  *emph-quad-list*)))))))

(progn
  (reset-fps-counter)
 (glfw:do-window (:title "bla" :width 512 :height 512)
     ()
   (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
     (return-from glfw::do-open-window))
   (draw)))

