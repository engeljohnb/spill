(in-package :spill)

(defun create-rect (x y w h)
  (list :x x :y y :w w :h h))

(defun spillp-convert-rect (rect)
  (sdl2:make-rect (getf rect :x)
                  (getf rect :y)
                  (getf rect :w)
                  (getf rect :h)))

(defun rect-collide-p (rect-1 rect-2)
  (let ((left-1 (getf rect-1 :x))
        (left-2 (getf rect-2 :x))
        (right-1 (+ (getf rect-1 :x) (getf rect-1 :w)))
        (right-2 (+ (getf rect-2 :x) (getf rect-2 :w)))
        (top-2 (getf rect-2 :y))
        (bottom-1 (+ (getf rect-1 :y) (getf rect-1 :h)))
        (bottom-2 (+ (getf rect-2 :y) (getf rect-2 :h))))
    (and (< left-1 right-2)
         (> right-1 left-2)
         (< top-2 bottom-2)
         (> bottom-1 top-2))))

(defun point-collide-p (rect x y)
  (let ((left (getf rect :x))
        (right (+ (getf rect :x) (getf rect :w)))
        (top (getf rect :y))
        (bottom (+ (getf rect :y) (getf rect :h))))
    (and (> x left)
         (< x right)
         (> y top)
         (< y bottom))))

(defun fill-surface (surface color)
  (let* ((split-color (break-color color))
         (r (getf split-color :r))
         (g (getf split-color :g))
         (b (getf split-color :b))
         (a (getf split-color :a))
         (prev-color (break-color (sdl2:get-render-draw-color (getf surface :sdl-renderer)))))
   (sdl2:set-render-target (getf surface :sdl-renderer) (getf surface :sdl-texture))
   (sdl2:set-render-draw-color (getf surface :sdl-renderer) r g b a)
   (sdl2:render-clear (getf surface :sdl-renderer))
   (sdl2:set-render-draw-color (getf surface :sdl-renderer)
                               (getf prev-color :r)
                               (getf prev-color :g)
                               (getf prev-color :b)
                               (getf prev-color :a))
   (sdl2:set-render-target (getf surface :sdl-renderer) nil)
   surface))
                               
(defun create-surface (window x y w h)
  (let ((sdl-texture (sdl2:create-texture (getf window :sdl-renderer) :argb8888 :target w h)))
    (sdl2:set-texture-blend-mode sdl-texture :blend)
  (list :sdl-texture sdl-texture 
        :sdl-renderer (getf window :sdl-renderer)
        :rect (create-rect x y w h)
        :window window)))

(defun blit-to-surface (source dest source-rect dest-rect)
  (let ((sdl-dest-rect
          (if (not dest-rect) 
              (spillp-convert-rect (getf source :rect))
              (spillp-convert-rect dest-rect)))
        (sdl-source-rect
          (if (not source-rect) 
              (sdl2:make-rect 
                          0
                          0
                          (getm source :rect :w)
                          (getm source :rect :h))
              (spillp-convert-rect source-rect))))
  (sdl2:set-render-target (getf dest :sdl-renderer) (getf dest :sdl-texture))
  (sdl2:render-copy (getf dest :sdl-renderer) 
                    (getf source :sdl-texture)
                    :source-rect sdl-source-rect
                    :dest-rect sdl-dest-rect)
  (sdl2:set-render-target (getf dest :sdl-renderer) nil)))
  
(defun blit-to-window (source dest source-rect dest-rect)
  (let ((sdl-dest-rect
           (if (not dest-rect) 
               (spillp-convert-rect (getf source :rect))
               (spillp-convert-rect dest-rect)))
        (sdl-source-rect
           (if (not source-rect) 
               (sdl2:make-rect 
                        0 
                        0 
                        (getm source :rect :w)
                        (getm source :rect :h))
               (spillp-convert-rect source-rect))))
  (sdl2:render-copy (getf dest :sdl-renderer) 
                    (getf source :sdl-texture) 
                    :source-rect sdl-source-rect 
                    :dest-rect sdl-dest-rect)))
                          
(defun blit (source dest &key (source-rect nil) (dest-rect nil))
  (cond ((member :sdl-window dest)
         (blit-to-window source dest source-rect dest-rect))
        ((member :sdl-texture dest)
         (blit-to-surface source dest source-rect dest-rect))
	((member :surface dest)
	 (if (member :surface source)
	     (blit-to-surface (getf source :surface) (getf dest :surface))
	     (blit-to-surface source :surface (getf dest :surface))))
        (t (progn (format t "blit error: invalid surface: ~%")
		  (print dest)
		  (fresh-line)
		  (invoke-debugger)))))

(defun free-surface (surface)
  (sdl2:destroy-texture (getf surface :sdl-texture)))
 

(defun draw-rect (dest color rect &optional (filled-p t))
  (let ((split-color (break-color color))
        (prev-color (break-color (sdl2:get-render-draw-color (getf dest :sdl-renderer))))
        (fun 'sdl2:render-draw-rect))
   (if filled-p
       (setf fun 'sdl2:render-fill-rect))
   (if (member :sdl-texture dest)
       (progn
         (sdl2:set-render-target (getf dest :sdl-renderer) (getf dest :sdl-texture))
         (sdl2:set-render-draw-color (getf dest :sdl-renderer)
                                    (getf split-color :r)
                                    (getf split-color :g)
                                    (getf split-color :b)
                                    (getf split-color :a))
         (eval  `(,fun ,(getf dest :sdl-renderer) ,(spillp-convert-rect rect)))
         (sdl2:set-render-draw-color (getf dest :sdl-renderer)
                                    (getf prev-color :r)
                                    (getf prev-color :g) 
                                    (getf prev-color :b)
                                    (getf prev-color :a))
         (sdl2:set-render-target (getf dest :sdl-renderer) nil)))
   (if (member :sdl-window dest)
       (progn
         (sdl2:set-render-draw-color (getf dest :sdl-renderer)
                                     (getf split-color :r)
                                     (getf split-color :g)
                                     (getf split-color :b)
                                     (getf split-color :a))
         (eval `(,fun ,(getf dest :sdl-renderer) ,(spillp-convert-rect rect)))
         (sdl2:set-render-draw-color (getf dest :sdl-renderer)
                                     (getf prev-color :r)
                                     (getf prev-color :g)
                                     (getf prev-color :b)
                                     (getf prev-color :a))))))

(defun draw-simple-triangle (dest apex-x apex-y height color &optional (direction 'right))
  (let ((split-color (break-color color))
        (prev-color (break-color (sdl2:get-render-draw-color (getf dest :sdl-renderer))))
        (current-w 0)
        (fun #'+))
    (if (member :sdl-texture dest)
        (sdl2:set-render-target (getf dest :sdl-renderer) (getf dest :sdl-texture))
        (sdl2:set-render-target (getf dest :sdl-renderer) nil))
    (if (eq direction 'left)
        (setf fun #'-))
    (sdl2:set-render-draw-color (getf dest :sdl-renderer)
                                (getf split-color :r)
                                (getf split-color :g)
                                (getf split-color :b)
                                (getf split-color :a))
    (loop for i from apex-y to (+ apex-y height) collect
          (progn
           (sdl2:render-draw-line (getf dest :sdl-renderer) apex-x i (funcall fun apex-x current-w) i)
           (incf current-w)))
    (sdl2:set-render-target (getf dest :sdl-renderer) nil)
    (sdl2:set-render-draw-color (getf dest :sdl-renderer)
                                 (getf prev-color :r)
                                 (getf prev-color :g)
                                 (getf prev-color :b)
                                 (getf prev-color :a))))

(defun default-up-surface (window width height name font-path config-colors border-width &optional (icon nil))
  (let ((surface (create-surface window 0 0 width height))
        (font (open-font 
              font-path
              (round (* height 0.5)))))
    (fill-surface surface (getf config-colors :base-color))
    (draw-rect surface 
                     (getf config-colors :top-border)
                     (create-rect 0 0 width border-width))
    (draw-rect surface 
                     (getf config-colors :top-border)
                     (create-rect 0 0 border-width height))
    (draw-rect surface 
                     (getf config-colors :bottom-border)
                     (create-rect (- width border-width) border-width border-width height))
    (draw-rect surface 
                     (getf config-colors :bottom-border)
                     (create-rect border-width (- height border-width) width border-width))
    (draw-simple-triangle surface border-width (- height border-width) border-width (getf config-colors :bottom-border) 'left)
    (draw-simple-triangle surface width 0 border-width (getf config-colors :bottom-border) 'left)
    (if icon
        (blit surface icon)
        (if name (draw-text surface font name :color (getf config-colors :text-color))))
    (close-font font)
    surface))

(defun default-down-surface (window width height name font-path config-colors border-width &optional (icon nil))
  (let ((surface (create-surface window 0 0 width height))
        (font (open-font 
              font-path
              (round (* height 0.5)))))
    (fill-surface surface (getf config-colors :base-color))
    (draw-rect surface (getf config-colors :top-border)
                     (create-rect 0 0 width border-width))
    (draw-rect surface (getf config-colors :top-border)
                     (create-rect 0 0 border-width height))
    (draw-rect surface (getf config-colors :bottom-border)
                     (create-rect (- width border-width) border-width border-width height))
    (draw-rect surface (getf config-colors :bottom-border)
                     (create-rect border-width (- height border-width) width border-width))
    (draw-simple-triangle surface border-width (- height border-width) border-width (getf config-colors :bottom-border) 'left)
    (draw-simple-triangle surface width 0 border-width (getf config-colors :bottom-border) 'left)
    (if icon
        (blit surface icon)
        (if name (draw-text surface font name :color (getf config-colors :text-color))))
    (close-font font)
    surface))

(defun create-image (window x y w h)
  (let ((image (list :surface (create-surface window x y w h)
		     :window window
		     :default-rect (create-rect x y w h)
		     :callbacks nil
		     :callback-data nil
		     :selected nil
		     :active nil)))
    image))
