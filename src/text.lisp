(in-package :spill)

(defparameter *controller-typing-page* nil)
(defparameter *ctp-init* nil)

(defparameter *key-row-0* (list "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "<-"))
(defparameter *key-row-1* (list "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"))
(defparameter *key-row-2* (list "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "return"))
(defparameter *key-row-3* (list "z" "x" "c" "v" "b" "n" "m" "," "." "/" "shift"))
(defparameter *key-row-4* (list " "))

(defparameter *shift-key-row-0* (list "~" "!" "@" "#" "#" "%" "^" "&" "*" "(" ")" "_" "+" "<-"))
(defparameter *shift-key-row-1* (list "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"))
(defparameter *shift-key-row-2* (list "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "'" "return"))
(defparameter *shift-key-row-3* (list "Z" "X" "C" "V" "B" "N" "M" "," "." "?" "shift"))
(defparameter *shift-key-row-4* (list " "))
(defparameter *stupid-string-memory* "")

(defun open-font (filename size)
   (list :sdl-font (sdl2-ttf:open-font filename size)
        :size size))

(defun close-font (font)
  (sdl2-ttf:close-font (getf font :sdl-font)))

(defun get-text-size (font text)
  (if (not text)
      (create-rect 0 0 0 0)
      (create-rect 0 0
                     (first 
                       (multiple-value-list (sdl2-ttf:size-text (getf font :sdl-font) text)))
                     (second 
                       (multiple-value-list (sdl2-ttf:size-text (getf font :sdl-font) text))))))

(defun create-text-surface (window font text &key (color #xff000000))
  (let* ((split-color (break-color color))
         (sdl-surface (sdl2-ttf:render-text-blended 
                              (getf font :sdl-font)
			      text
                              (getf split-color :r)
                              (getf split-color :g)
                              (getf split-color :b)
                              (getf split-color :a)))
          (sdl-texture (sdl2:create-texture-from-surface (getf window :sdl-renderer) sdl-surface))
          (final-texture (sdl2:create-texture (getf window :sdl-renderer)
                                              :argb8888
                                              :target
                                              (getf (get-text-size font text) :w)
                                              (getf (get-text-size font text) :h))))
     (sdl2:set-texture-blend-mode sdl-texture :blend)
     (sdl2:set-texture-blend-mode final-texture :blend)
     (sdl2:set-render-target (getf window :sdl-renderer) final-texture)
     (sdl2:render-copy (getf window :sdl-renderer) sdl-texture)
     (sdl2:set-render-target (getf window :sdl-renderer) nil)
           (sdl2:free-surface sdl-surface)
           (sdl2:destroy-texture sdl-texture)
     (list :sdl-texture final-texture
           :sdl-renderer (getf window :sdl-renderer)
           :rect (get-text-size font text))))

(defun draw-text (surface font text &key (x nil) (y nil) (color #xff000000))
  (if (not x) (setf x 'text-centered))
  (if (not y) (setf y 'text-centered))
  (let ((tx 0)
        (ty 0))
   (if (numberp x) (setf tx x))
   (if (numberp y) (setf ty y))
   (cond ((eq x 'text-centered)
          (setf tx (middle (getm surface :rect :w)
                          (getf (get-text-size font text) :w))))
         ((eq x 'text-right)
          (setf tx (- (getm surface :rect :w) (getf (get-text-size font text) :w)))))
   (cond ((eq y 'text-centered)
         (setf ty (middle (getm surface :rect :h)
                          (getf (get-text-size font text) :h))))
         ((eq y 'text-right)
          (setf ty (- (getm surface :rect :h) (getf (get-text-size font text) :h)))))
   (let ((text-surface (create-text-surface surface font text :color color)))
     (setf (getm text-surface :rect :x) tx)
     (setf (getm text-surface :rect :y) ty)
     (blit text-surface surface)
         (free-surface text-surface))))

(defun last-char (_string)
  (char _string (- (length _string) 1)))

(defun position->index (typing-bar)
  (let ((relative-mouse-x (getf (get-relative-mouse-pos typing-bar) :x))
	(current-distance 0)
	(current-index 0)
	(closest-distance 10000)
	(closest-index -1)
	(font (getf typing-bar :font))
	(_string (getf typing-bar :string)))
    (map 'string
	 (lambda (current-char)
	  (if (<= (abs (- current-distance relative-mouse-x)) closest-distance)
	      (progn
	       (setf closest-distance (abs (- current-distance relative-mouse-x)))
	       (setf closest-index current-index)))
           (incf current-distance 
		 (getf (get-text-size font (string current-char)) :w))
	   (incf current-distance (getf typing-bar :char-spacing))
	   (incf current-index)
	   current-char)
	 _string)
    closest-index))

(defun index->position (typing-bar index)
  (let ((current-index 0)
	(current-distance 0)
	(font (getf typing-bar :font))
	(_string (getf typing-bar :string)))
    (map 'string
	 (lambda (current-char)
	   (unless (>= current-index index)
	           (progn
	             (incf current-distance
		           (getf (get-text-size font (string current-char)) :w))
	             (incf current-distance (getf typing-bar :char-spacing))))
	   (incf current-index)
	   current-char)
	 _string)
    current-distance))

(defun insert-string (index dest-string insert-string)
  (cond ((empty-string-p dest-string) insert-string)
        ((empty-string-p insert-string) dest-string)
        ((>= index (length dest-string)) (concatenate 'string dest-string insert-string))
        (t
         (progn
           (let ((string-before-index (subseq dest-string 0 index))
                 (string-after-index (subseq dest-string index (length dest-string))))
             (concatenate 'string string-before-index insert-string string-after-index))))))

(defun draw-typing-bar-text (typing-bar)
  (blit (getf typing-bar :blank-select-surface) 
	      (getf typing-bar :select-surface))
  (blit (getf typing-bar :blank-deselect-surface) 
	      (getf typing-bar :deselect-surface))
  (let* ((current-x (+ (getf typing-bar :char-spacing) (getm typing-bar :cursor-surface :rect :w)))
         (current-y (getm typing-bar :cursor-surface :rect :y))
	 (i 0))
   (map 'string
        #'(lambda (current-char)
           (let* ((width (+ (getf (get-text-size (getf typing-bar :font) (string current-char)) :w) (getf typing-bar :char-spacing)))
                  (height (getf (get-text-size (getf typing-bar :font) (string current-char)) :h))
                  (rect (create-rect current-x 
					   current-y
					   width 
					   height))
                  (color (getf typing-bar :text-color)))
             (if (and (>= (getf typing-bar :selected-chars-start) 0)
                      (>= (getf typing-bar :selected-chars-end) 0))
                 (if (and (< i (max (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end)))
                          (>= i (min (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end))))
                     (progn
                          (setf color (getf typing-bar :select-text-color))
                          (blit (getf typing-bar :highlight-surface) 
                                      (getf typing-bar :deselect-surface) 
                                      :source-rect rect
                                      :dest-rect rect)
                          (blit (getf typing-bar :highlight-surface)
                                      (getf typing-bar :select-surface)
                                      :source-rect rect
                                      :dest-rect rect))))
             (draw-text (getf typing-bar :deselect-surface)
                              (getf typing-bar :font)
                              (string (char (getf typing-bar :string) i))
                              :x (getf rect :x)
                              :y (getf rect :y)
                              :color color)
	     current-char))
	(getf typing-bar :string)))

  (let* ((current-x (+ (getf typing-bar :char-spacing) (getm typing-bar :cursor-surface :rect :w)))
         (current-y (getm typing-bar :cursor-surface :rect :y))
	 (i 0))
   (map 'string
        #'(lambda (current-char)
           (let* ((width (+ (getf (get-text-size (getf typing-bar :font) (string current-char)) :w) (getf typing-bar :char-spacing)))
                  (height (getf (get-text-size (getf typing-bar :font) (string current-char)) :h))
                  (rect (create-rect current-x current-y width height))
                  (color (getf typing-bar :text-color)))
             (if (and (>= (getf typing-bar :selected-chars-start) 0)
                      (>= (getf typing-bar :selected-chars-end) 0))
                 (if (and (< i (max (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end)))
                          (>= i (min (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end))))
                     (progn
                          (setf color (getf typing-bar :select-text-color))
                          (blit (getf typing-bar :highlight-surface) 
                                      (getf typing-bar :deselect-surface) 
                                      :source-rect rect
                                      :dest-rect rect)
                          (blit (getf typing-bar :highlight-surface)
                                      (getf typing-bar :select-surface)
                                      :source-rect rect
                                      :dest-rect rect))))
             (draw-text (getf typing-bar :deselect-surface)
                              (getf typing-bar :font)
                              (string (char (getf typing-bar :string) i))
                              :x (getf rect :x)
                              :y (getf rect :y)
                              :color color)
             (draw-text (getf typing-bar :select-surface)
                              (getf typing-bar :font)
                              (string (char (getf typing-bar :string) i))
                              :x (getf rect :x)
                              :y (getf rect :y)
                              :color color)
             (incf current-x width)
             (incf i)
             current-char))
	(getf typing-bar :string))))

(defun get-selected-string (typing-bar)
  (let ((start (min (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end)))
	(end (max (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end))))
  (if (= end (length (getf typing-bar :string)))
      (setf end (- (length (getf typing-bar :string)) 1)))
  (cond ((= start end) nil)
	((or (< end 0) (< start 0)) nil)
	(t
	  (subseq (getf typing-bar :string) start (+ end 1))))))

(defun delete-from-typing-bar-string (typing-bar)
  (let ((old-string (getf typing-bar :string))
        (new-string (getf typing-bar :string))
        (index (getf typing-bar :cursor-index))
	(i 0)
	(select-start (min (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end)))
	(select-end (max (getf typing-bar :selected-chars-start) (getf typing-bar :selected-chars-end))))
    (cond 
      ((< (getf typing-bar :selected-chars-end) 0)
          (setf new-string
                (concatenate 'string
                             (subseq old-string 0 index)
                             (subseq old-string (+ index 1) (length old-string)))))
      (t
	(progn
	  (let ((concatenation ""))
	    (map 'string
	         (lambda (current-char)
	      	   (unless (and (>= i select-start)
		      	        (< i select-end))
			   (setf concatenation (concatenate 'string concatenation (string current-char))))
		   (incf i)
		   current-char)
		 old-string)
	    (setf new-string concatenation)))))
    (if (> (getf typing-bar :selected-chars-end) -1)
        (setf (getf typing-bar :cursor-index) select-start))
    (setf (getf typing-bar :selected-chars-start) -1)
    (setf (getf typing-bar :selected-chars-end) -1)
    (setf (getf typing-bar :string) new-string)))

(defun process-typing-bar (typing-bar)
  (if (getf *fleeting-input* :key)
     (progn
       (blit (getf typing-bar :blank-select-surface) 
              (getf typing-bar :select-surface)
              :source-rect (getm typing-bar :cursor-surface :rect)
              :dest-rect (getm typing-bar :cursor-surface :rect))
       (blit (getf typing-bar :blank-deselect-surface) 
              (getf typing-bar :deselect-surface)
              :source-rect (getm typing-bar :cursor-surface :rect)
              :dest-rect (getm typing-bar :cursor-surface :rect))
       (let* ((key (getf *fleeting-input* :key))
              (font (getf typing-bar :font))
              (char-rect (get-text-size font key)))
         (declare (ignorable char-rect))
         (cond ((or (string-equal key "left shift") (string-equal key "right shift")) nil)
               ((string-equal key "backspace")
                (if (and (> (length (getf typing-bar :string)) 0) 
			 (or (> (getf typing-bar :cursor-index) 0)
			     (and (> (getf typing-bar :cursor-index) -1)
				  (> (getf typing-bar :selected-chars-end) -1))))
                    (progn
                      (decf (getf typing-bar :cursor-index))
                      (delete-from-typing-bar-string typing-bar)
                      (draw-typing-bar-text typing-bar))))
	       ((> (length key) 1) nil)
               (t (progn
                   (setf (getf typing-bar :string) (insert-string (getf typing-bar :cursor-index) (getf typing-bar :string) key))
                   (incf (getf typing-bar :cursor-index))
                   (draw-typing-bar-text typing-bar))))
         (setf (getm typing-bar :cursor-surface :rect :y) (middle (getm typing-bar :default-rect :h) (round (* 0.75 (getm typing-bar :default-rect :h))))))))

  (setf (getm typing-bar :cursor-surface :rect :x)
	(index->position typing-bar (getf typing-bar :cursor-index)))
  (if (= (mod *frames* 24) 0)
      (if (getf typing-bar :show-cursor-p)
          (setf (getf typing-bar :show-cursor-p) nil)
          (setf (getf typing-bar :show-cursor-p) t)))
  (if (getf *fleeting-input* :key)
      (setf (getf typing-bar :show-cursor-p) t))
  (if (getf typing-bar :show-cursor-p)
      (progn (blit (getf typing-bar :cursor-surface) (getf typing-bar :deselect-surface))
             (blit (getf typing-bar :cursor-surface) (getf typing-bar :select-surface)))
      (progn (blit (getf typing-bar :blank-select-surface)
                         (getf typing-bar :select-surface)
                         :source-rect (getm typing-bar :cursor-surface :rect)
                         :dest-rect (getm typing-bar :cursor-surface :rect))
             (blit (getf typing-bar :blank-deselect-surface)
                         (getf typing-bar :deselect-surface)
                         :source-rect (getm typing-bar :cursor-surface :rect)
                         :dest-rect (getm typing-bar :cursor-surface :rect))))
  (if (and (> (getf typing-bar :selected-chars-start) -1)
	   (> (getf typing-bar :selected-chars-end) -1))
      (progn
        (blit (getf typing-bar :highlight-surface)
		    (getf typing-bar :select-surface)
		    :source-rect (getm typing-bar :cursor-surface :rect)
		    :dest-rect (getm typing-bar :cursor-surface :rect))
        (blit (getf typing-bar :highlight-surface)
  		    (getf typing-bar :deselect-surface)
		    :source-rect (getm typing-bar :cursor-surface :rect)
		    :dest-rect (getm typing-bar :cursor-surface :rect)))))

(defun typing-bar-click-callback (typing-bar)
  (setf (getf typing-bar :active) t)
  (draw-typing-bar-text typing-bar))

(defun typing-bar-click-and-drag-callback (typing-bar)
  (let ((index (position->index typing-bar))
	(text-width (getf (get-text-size (getf typing-bar :font) (getf typing-bar :string)) :w))
	(mouse-x (getf (get-relative-mouse-pos typing-bar) :x)))
    (if (>= mouse-x text-width)
        (setf index (length (getf typing-bar :string))))
    (unless (or (= index -1)
		(= index (getf typing-bar :cursor-index)))
            (progn
	      (setf (getf typing-bar :cursor-index) index)
	      (setf (getf typing-bar :selected-chars-end) index)))
    (draw-typing-bar-text typing-bar)))

(defun  typing-bar-mouse-down-callback (typing-bar)
  (setf (getf typing-bar :selected-chars-start) -1)
  (setf (getf typing-bar :selected-chars-end) -1)
  (let ((index (position->index typing-bar))
        (text-width (getf (get-text-size (getf typing-bar :font) (getf typing-bar :string)) :w))
	(mouse-x (getf (get-relative-mouse-pos typing-bar) :x)))
    (if (>= mouse-x text-width)
        (setf index (length (getf typing-bar :string))))
    (unless (= index -1)
            (progn
               (setf (getf typing-bar :cursor-index) index)
	       (setf (getf typing-bar :selected-chars-start) index)))
    (draw-typing-bar-text typing-bar)))

(defun typing-bar-controller-click-callback (typing-bar)
  (setf (getf typing-bar :active) t))

(defun create-typing-bar (window x y width height &key (controller-input-p nil) (config *default-typing-bar-config*)
				(input-config *default-input-config*))
  (let ((typing-bar 
          (list :deselect-surface (funcall (getf config :surface)
                                  window
                                  width
                                  height
                                  nil
                                  (getf config :font)
                                  (getm config :colors :typing-bar-colors)
                                  (getf config :border-width))
                :select-surface (funcall (getf config :surface)
                                         window
                                         width
                                         height
                                         nil
                                         (getf config :font)
                                         (getm config :colors :typing-bar-select-colors)
                                         (getf config :border-width))
                :blank-deselect-surface nil
                :blank-select-surface nil
                :text-color (getf config :text-color)
                :select-text-color (getf config :select-text-color)
                :highlight-surface (create-surface window 0 0 width height)
                :controller-input controller-input-p
                :window window
                :surface nil
                :active nil
                :show-cursor-p nil
                :selected nil
                :selected-chars-start -1
                :selected-chars-end -1
                :string ""
                :default-rect nil
                :font (open-font (getf config :font) (round (* height 0.75)))
                :char-spacing (getf config :char-spacing)
                :cursor-index 0
                :cursor-surface (create-surface window
                                                      0 
                                                      (middle height (round (* height 0.75)))
                                                      2
                                                      (round (* height 0.75)))
                :callbacks (if (eq (getf input-config :source) 'key-and-mouse)
			       (list (cons 'click #'typing-bar-click-callback)
                                     (cons 'selected #'(lambda (typing-bar) (setf (getf typing-bar :surface) (getf typing-bar :select-surface))))
                                     (cons 'deselected #'(lambda (typing-bar) (setf (getf typing-bar :surface) (getf typing-bar :deselect-surface))))
                                     (cons 'active #'process-typing-bar)
                                     (cons 'click-away #'(lambda (typing-bar) 
                                                           (setf (getf typing-bar :active) nil)
                                                           (blit (getf typing-bar :blank-deselect-surface) 
                                                                       (getf typing-bar :deselect-surface) 
                                                                       :source-rect (getm typing-bar :cursor-surface :rect) 
                                                                       :dest-rect (getm typing-bar :cursor-surface :rect))
                                                           (blit (getf typing-bar :blank-select-surface)
                                                                       (getf typing-bar :select-surface)
                                                                       :source-rect (getm typing-bar :cursor-surface :rect)
                                                                       :dest-rect (getm typing-bar :cursor-surface :rect))))
                                     (cons 'click-and-drag #'typing-bar-click-and-drag-callback)
                                     (cons 'fleeting-lmb-down #'typing-bar-mouse-down-callback))

			       (list (cons 'click #'typing-bar-controller-click-callback)
				     (cons 'selected #'(lambda (typing-bar) (setf (getf typing-bar :surface) (getf typing-bar :select-surface))))
				     (cons 'deselected #'(lambda (typing-bar) (setf (getf typing-bar :surface) (getf typing-bar :deselect-surface))))
				     (cons 'page-enter #'(lambda (typing-bar) 
							   (setf (getf typing-bar :active) nil)
							   (setf (getf typing-bar :selected) nil)
							   (setf (getf typing-bar :string) *stupid-string-memory*)
							   (setf (getf typing-bar :surface) (getf typing-bar :deselect-surface))
							   (draw-typing-bar-text typing-bar)
							   (setf *stupid-string-memory* "")))))

                :type 'typing-bar
		:prev-page nil
		:sender nil
		:input-source (getf input-config :source)
		:freed nil)))
    (setf (getf typing-bar :callback-data) (list (cons 'click typing-bar)
                                                (cons 'selected typing-bar)
                                                (cons 'deselected typing-bar)
                                                (cons 'active typing-bar)
                                                (cons 'click-away typing-bar)
                                                (cons 'click-and-drag typing-bar)
                                                (cons 'fleeting-lmb-down typing-bar)
						(cons 'page-enter typing-bar)))
    (setf (getf typing-bar :surface) (getf typing-bar :deselect-surface))
    (setf (getf typing-bar :default-rect) (create-rect x y width height))
    (setf (getf typing-bar :blank-deselect-surface) (create-surface window 0 0 width height))
    (setf (getf typing-bar :blank-select-surface) (create-surface window 0 0 width height))
    (fill-surface (getf typing-bar :highlight-surface) (getf config :highlight-color))
    (fill-surface (getf typing-bar :cursor-surface) #xff000000)
    (blit (getf typing-bar :select-surface) (getf typing-bar :blank-select-surface))
    (blit (getf typing-bar :deselect-surface) (getf typing-bar :blank-deselect-surface))
    typing-bar))

(defun free-typing-bar (typing-bar)
  (unless (getf typing-bar :freed)
      (progn
        (free-surface (getf typing-bar :deselect-surface))
        (free-surface (getf typing-bar :select-surface))
	(setf (getf typing-bar :freed) t))))
