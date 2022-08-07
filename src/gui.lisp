; Things all widgets need (probably an incomplete list)
        ; :type
        ; :default-rect
        ; :surface
        ; :callbacks
        ; :callback-data
        ; :selected
(in-package :spill)

(defparameter *prev-time* 0)

(defun create-widget-grid (widgets-per-row x y x-spacing y-spacing)
  (list :widgets-per-row widgets-per-row
        :x x
        :y y
        :x-spacing x-spacing
        :y-spacing y-spacing))

(defun arrange-widgets (widgets widget-grid)
  (let ((current-x (getf widget-grid :x))
        (current-y (getf widget-grid :y))
        (current-row-index 0)
        (total-row-height 0))
    (dolist (widget widgets)
      (if (> (getm widget :default-rect :h) total-row-height)
          (setf total-row-height (getm widget :default-rect :h))))
    (incf total-row-height (getf widget-grid :y-spacing))
    (dotimes (i (length widgets) t)
      (let ((widget (nth i widgets)))
         (setf (getm widget :default-rect :x) current-x)
         (setf (getm widget :default-rect :y) current-y)
         (incf current-x (+ (getf widget-grid :x-spacing) (getm widget :default-rect :w)))
         (incf current-row-index)
         (if (>= current-row-index (getf widget-grid :widgets-per-row))
             (progn (setf current-x (getf widget-grid :x))
                    (setf current-row-index 0)
                    (incf current-y total-row-height)))))))

(defun widget-callback (widget trigger)
  (let ((functions (remove-if-not #'(lambda (fun) (eq (car fun) trigger)) (getf widget :callbacks)))
        (data (remove-if-not #'(lambda (datum) (eq (car datum) trigger)) (getf widget :callback-data))))
    (loop for i from 0 to (length functions) collect
     (progn
      (let ((fun (nth i functions))
            (datum (nth i data)))
        (if (and fun datum)
         (funcall (cdr fun) (cdr datum))))))))

(defun get-page (gui page-name)
  (if gui
      (let ((page nil))
        (dolist (current-page (getf gui :pages))
          (if (eq page-name (getf current-page :name))
              (setf page current-page)))
        (if (not page)
            (setf page (get-page gui 'default)))
        page)))

(defun getm-page-iter (item &rest page-names)
  `(get-page ,item ,(first page-names)))

(defmacro getm-page (gui &rest page-names)
  (let ((result gui))
    (car 
     (last
      (loop for mem in page-names
       collect
       (setf result (getm-page-iter result mem)))))))

(defun set-current-page (gui page-name)
  (dolist (widget (getm gui :current-page :widgets))
    (widget-callback widget 'page-exit))
  (setf (getf gui :prev-page) (getf gui :current-page))
  (setf (getf gui :current-page) (get-page gui page-name))
  (dolist (widget (getm gui :current-page :widgets))
    (widget-callback widget 'page-enter)))

(defun when-running (gui page-name fun data)
  (if (eq page-name 'all-pages)
    (dolist (current-page (getf gui :pages))
        (push (cons 'always fun) (getf current-page :callbacks))
        (push (cons 'always data) (getf current-page :callback-data)))
     (progn (let ((page (get-page gui page-name)))
       (push (cons 'always fun) (getf page :callbacks))
       (push (cons 'always data) (getf page :callback-data))))))

(defun add-page (gui name &key (render-target (getf gui :render-target)) (render-rect nil))
  (if (eq (getf gui :type) 'tab)
      (setf render-rect (getf gui :tab-frame-rect)))
  (push (list :name name
              :render-target render-target
	      :window (getf gui :window)
              :widgets nil
	      :prev-selected-widget nil
              :selection-wait-frames 15
              :selection-wait-counter 0
              :selection-wait-offset 0
	      :callbacks nil
	      :callback-data nil
	      :default-rect (if render-rect
			        render-rect
				(getf render-target :rect))
	      :pages nil 
	      :current-page nil)
        (getf gui :pages)))

(defun add-callback (widget trigger callback data)
  (push (cons trigger callback) (getf widget :callbacks))
  (push (cons trigger data) (getf widget :callback-data)))

(defun remove-callback (widget trigger)
  (dolist (callback (getf widget :callbacks))
    (if (eq (car callback) trigger)
        (setf (getf widget :callbacks)
	      (remove callback (getf widget :callbacks)))))
  (dolist (datum (getf widget :callback-data))
    (if (eq (car datum) trigger)
        (setf (getf widget :callback-data)
	      (remove datum (getf widget :callback-data))))))

;(defun create-typing-page-row (gui name-list size)
;  (let ((buttons nil))
;    (dolist (key name-list)
;      (unless (or (string-equal key " ")
;		  (string-equal key "shift")
;		  (string-equal key "return"))
;              (setf buttons (append buttons (list (create-button (getf gui :window) size size key)))))
;      (cond ((string-equal key " ")
;	     (setf buttons (append buttons (list (create-button (getf gui :window) (* size 10) size key)))))
;	    ((string-equal key "return")
;	     (setf buttons (append buttons (list (create-button (getf gui :window) (* size 2) size key)))))
;	    ((string-equal key "shift")
;	     (setf buttons (append buttons (list (create-button (getf gui :window) (* size 2) size key)))))))
;    (dolist (button buttons)
;      (if (= (length (getf button :name)) 1)
;	     (add-callback button
;				 'fleeting-lmb-down
;				 #'(lambda (button)
;				     (push (getf button :name) (getf *fleeting-input* :keys-down))
;				     (setf (getf *fleeting-input* :key) (getf button :name)))
;				 button))
;      (if (string-equal "<-" (getf button :name))
;	  (add-callback button
;			      'fleeting-lmb-down
;			      #'(lambda (button)
;				  (declare (ignorable button))
;				  (push "backspace" (getf *fleeting-input* :keys-down))
;				  (setf (getf *fleeting-input* :key) "backspace"))
;			      button))
;      (if (string-equal " " (getf button :name))
;	  (add-callback button
;			      'fleeting-lmb-down
;			      #'(lambda (button)
;				  (declare (ignorable button))
;				  (push "space" (getf *fleeting-input* :keys-down))
;				  (setf (getf *fleeting-input* :key) "space"))
;			      button)))
;    buttons))
;
(defun add-widget (gui page-name widget)
  (let ((page (get-page gui page-name)))
    (if (eq (getf widget :type) 'tab-frame)
        (progn (setf (getf widget :parent-page-rect)
		     (getf page :default-rect))
	(dolist (tab (getf widget :tabs))
	  (dolist (tab-page (getf tab :pages))
	    (setf (getf tab-page :default-rect)
		  (create-rect (getm page :default-rect :x)
			       (+ (getm page :default-rect :y) (getm tab :tab-height))
			       (getm page :default-rect :w)
			       (- (getm page :default-rect :h) (getm tab :tab-height))))))))
    (push widget (getf page :widgets))
    (widget-callback widget 'page-enter)))

;(defun create-controller-typing-page (gui)
;  (let* ((button-size (floor (/ (getm gui :window :width) 
;				(+ (length *key-row-3*) 4.5))))
;	 (all-rows (list
;	            (create-typing-page-row gui *key-row-0* button-size)
;                    (create-typing-page-row gui *key-row-1* button-size)
;                    (create-typing-page-row gui *key-row-2* button-size)
;                    (create-typing-page-row gui *key-row-3* button-size)
;                    (create-typing-page-row gui *key-row-4* button-size)))
;        (all-shift-rows (list
;   	                  (create-typing-page-row gui *shift-key-row-0* button-size)
;   	                  (create-typing-page-row gui *shift-key-row-1* button-size)
;   	                  (create-typing-page-row gui *shift-key-row-2* button-size)
;   	                  (create-typing-page-row gui *shift-key-row-3* button-size)
;   	                  (create-typing-page-row gui *shift-key-row-4* button-size)))
;	(x 5)
;	(shift-x 5)
;	(y (+ 5 (round (/ (getm gui :window :height) 16))))
;	(shift-y (+ 5 (round (/ (getm gui :window :height) 16))))
;	(x-spacing 5)
;	(y-spacing 5)
;        (typing-bar (create-typing-bar (getf gui :window) 
;      			         x-spacing
;			         y-spacing
;			         (- (getm gui :window :width) (* x-spacing 2))
;			         (- y (* y-spacing 2)))))
;  (blit (getf typing-bar :deselect-surface) (getf typing-bar :select-surface))
;  (blit (getf typing-bar :blank-deselect-surface) (getf typing-bar :blank-select-surface))
;  (add-callback (name->button "shift" (fourth all-rows))
;		      'fleeting-lmb-down
;		      #'(lambda (gui) 
;			  (set-current-page gui 'gamepad-typing-bar-shift))
;		      gui)
;  (add-callback (name->button "shift" (fourth all-shift-rows))
;		      'fleeting-lmb-down
;		      #'(lambda (gui) 
;			  (set-current-page gui 'gamepad-typing-bar))
;		      gui)
;  (add-page gui 'gamepad-typing-bar)
;  (add-page gui 'gamepad-typing-bar-shift)
;  (remove-callback typing-bar 'page-enter)
;  (add-callback typing-bar
;		      'page-enter
;		      #'(lambda (typing-bar)
;			  (setf (getf typing-bar :active) t)
;			  (unless (getf typing-bar :prev-page)
;			     (setf (getf typing-bar :prev-page) (getf gui :prev-page))))
;		      typing-bar)
;  (add-callback typing-bar
;		      'page-exit
;		      #'(lambda (typing-bar)
;			  (setf (getf typing-bar :active) nil))
;		      typing-bar)
;  (add-callback typing-bar
;		      'active
;		      #'process-typing-bar
;		      typing-bar)
;  (add-callback typing-bar
;		      'selected
;		      #'(lambda (data)
;			  (let ((typing-bar (first data))
;				(gui (second data)))
;			  (setf (getf typing-bar :selected) nil)
;			  (setf (getm gui :current-page :prev-selected-widget :selected) t)))
;		      (list typing-bar gui))
;  (add-callback (get-page gui 'gamepad-typing-bar)
;		      'fleeting-controller-x-down
;		      #'(lambda (backspace)
;			  (widget-callback backspace 'fleeting-lmb-down))
;		      (name->button "<-" (first all-rows)))
;  (add-callback (get-page gui 'gamepad-typing-bar-shift)
;		      'fleeting-controller-x-down
;		      #'(lambda (backspace)
;			  (widget-callback backspace 'fleeting-lmb-down))
;		      (name->button "<-" (first all-shift-rows)))
;  (add-callback (get-page gui 'gamepad-typing-bar)
;		      'fleeting-controller-y-down
;		      #'(lambda (space)
;			  (widget-callback space 'fleeting-lmb-down))
;		      (name->button " " (fifth all-rows)))
;  (add-callback (get-page gui 'gamepad-typing-bar-shift)
;		      'fleeting-controller-y-down
;		      #'(lambda (space)
;			  (widget-callback space 'fleeting-lmb-down))
;		      (name->button " " (fifth all-shift-rows)))
;  (add-callback (name->button "return" (third all-rows))
;		      'fleeting-lmb-down
;		      #'(lambda (typing-bar)
;			  (setf *stupid-string-memory* (getf typing-bar :string))
;			  (setf (getf typing-bar :prev-page) nil)
;			  (set-current-page gui (getm typing-bar :prev-page :name)))
;			typing-bar)
;  (add-callback (name->button "return" (third all-shift-rows))
;		      'fleeting-lmb-down
;		      #'(lambda (typing-bar)
;			  (setf *stupid-string-memory* (getf typing-bar :string))
;			  (setf (getf typing-bar :prev-page) nil)
;			  (set-current-page gui (getm typing-bar :prev-page :name)))
;			typing-bar)
;
;  (add-widget gui 'gamepad-typing-bar typing-bar)
;  (add-widget gui 'gamepad-typing-bar-shift typing-bar)
;  (dolist (row all-rows)
;    (arrange-widgets row (create-widget-grid (length row) x y x-spacing y-spacing))
;    (incf y (+ (getm (first row) :default-rect :h) y-spacing))
;    (incf x (round (* (getm (first row) :default-rect :w) 0.25)))
;    (dolist (button row)
;      (add-widget gui 'gamepad-typing-bar button)))
;  (dolist (row all-shift-rows)
;    (arrange-widgets row (create-widget-grid (length row) shift-x shift-y x-spacing y-spacing))
;    (incf shift-y (+ (getm (first row) :default-rect :h) y-spacing))
;    (incf shift-x (round (* (getm (first row) :default-rect :w) 0.25)))
;    (dolist (button row)
;      (add-widget gui 'gamepad-typing-bar-shift button)))))

(defun create-gui (window &key (input-config *default-input-config*) (render-target nil) (render-rect nil))
  (let ((gui (list :window window
		   :type 'gui
                   :pages nil
                   :current-page nil
		   :prev-page nil)))
    (if (not render-target)
        (setf render-target window))
    (add-page gui 'default :render-target render-target :render-rect render-rect)
    (if (eq (getf input-config :source) 'gamepad)
        (create-controller-typing-page gui))
    (set-current-page gui 'default)
    gui))


;(defun get-closest-widget-on-grid (widgets target-rect direction)
;  (if (not direction) nil
;  (let ((closest-distance 10000)
;        (closest-widget nil)
;        (direction-check
;          (cond ((or (eq direction 'left) (eq direction'up)) #'<)
;                ((or (eq direction 'right) (eq direction 'down)) #'>)))
;        (variable-axis
;          (cond ((or (eq direction 'left) (eq direction 'right)) :x)
;                ((or (eq direction 'up) (eq direction 'down)) :y)))
;        (stable-axis
;          (cond ((or (eq direction 'left) (eq direction 'right)) :y)
;                ((or (eq direction 'up) (eq direction 'down)) :x))))
;    (dolist (widget widgets)
;      (let ((current-distance nil))
;       (if (= (getf target-rect stable-axis) (getm widget :default-rect stable-axis))
;           (cond ((= (getm widget :default-rect variable-axis) (getf target-rect variable-axis))
;                     (progn (setf closest-widget widget)
;                            (setf closest-distance 0)))
;                 ((funcall direction-check (getm widget :default-rect variable-axis) (getf target-rect variable-axis)) 
;                     (progn (setf current-distance (get-distance (getf widget :default-rect) target-rect))
;                            (if (< current-distance closest-distance)
;                                (progn (setf closest-widget widget)
;                                (setf closest-distance current-distance)))))))))
;    closest-widget)))

;(defun get-controller-selected-widget-iter (widgets target-rect)
;  (let ((closest-distance 10000)
;        (closest-widget nil))
;  (dolist (widget widgets)
;    (if (< (get-distance (getf widget :default-rect) target-rect) closest-distance)
;        (progn (setf closest-widget widget)
;               (setf closest-distance (get-distance (getf widget :default-rect) target-rect)))))
;  closest-widget))

;(defun update-selection-counter (page)
;  (if (not (controller-movement-p))
;      (progn
;        (setf (getf page :selection-wait-counter) 0)
;        (setf (getf page :selection-wait-offset) 0)
;        nil)
;      (if (fleeting-controller-movement-p) t
;        (if (> (+ (incf (getf page :selection-wait-offset)) (incf (getf page :selection-wait-counter))) (getf page :selection-wait-frames))
;            (progn
;              (if (> (getf page :selection-wait-offset) 10) (setf (getf page :selection-wait-offset) 10))
;              (setf (getf page :selection-wait-counter) 0)
;              t)
;            nil))))

;(defun get-controller-selected-widget (page prev-selected &optional (input-config *default-input-config*))
;  (declare (ignorable input-config))
;  (if (not (update-selection-counter page))
;      prev-selected 
;        (progn    
;            (let ((selected-widget nil)
;                  (target-rect nil)
;                  (direction nil))
;              (dolist (widget (getf page :widgets))
;                (if (getf widget :selected)
;                    (progn (setf selected-widget widget)
;                           (setf target-rect (copy-list (getf selected-widget :default-rect)))
;                           (if (controller-movement-p)
;                               (progn 
;                                       (if (getf *stable-input* :left)
;                                           (progn (setf direction 'left)
;                                                  (decf (getf target-rect :x) (getf target-rect :w))))
;                                       (if (getf *stable-input* :right)
;                                           (progn (setf direction 'right)
;                                                  (incf (getf target-rect :x) (getf target-rect :w))))
;                                       (if (getf *stable-input* 'up)
;                                           (progn (setf direction :up)
;                                                  (decf (getf target-rect :y) (getf target-rect :h))))
;                                       (if (getf *stable-input* :down)
;                                           (progn (setf direction 'down)
;                                                  (incf (getf target-rect :y) (getf target-rect :h)))))))))
;              (if (not target-rect) (setf target-rect (create-rect 0 0 20 20)))
;              (let ((closest-widget nil))
;                (setf closest-widget (get-closest-widget-on-grid (getf page :widgets) target-rect direction))
;                (if (not closest-widget) 
;                    (loop for i from 0 until
;                      (or (or 
;                            (> (+ (getf target-rect :x) (getf target-rect :w))  (getm page :window :width))
;                            (< (getf target-rect :x) 0)
;                            (> (+ (getf target-rect :y) (getf target-rect :h)) (getm page :window :height))
;                            (< (getf target-rect :y) 0))
;                          (and (not (eql closest-widget prev-selected))
;                               closest-widget))
;                      collect
;                      (progn
;                        (setf closest-widget (get-controller-selected-widget-iter (getf page :widgets) target-rect))
;                        (if (getf *stable-input* :left)
;                            (decf (getf target-rect :x) (getf target-rect :w)))
;                        (if (getf *stable-input* :right)
;                            (incf (getf target-rect :x) (getf target-rect :w)))
;                        (if (getf *stable-input* :up)
;                            (decf (getf target-rect :y) (getf target-rect :h)))
;                        (if (getf *stable-input* :down)
;                            (incf (getf target-rect :y) (getf target-rect :h))))))
;                (setf selected-widget closest-widget))
;              (if (not selected-widget) (setf selected-widget prev-selected))
;              selected-widget))))
                         
(defun get-selected-widget (page &optional (input-config *default-input-config*))
  (let ((selected-widget nil)
        (prev-selected nil)
	(mouse-pos (get-relative-mouse-pos page)))
   (dolist (widget (getf page :widgets))
     (if (getf widget :selected)
         (setf prev-selected widget)))
   (setf (getf page :prev-selected-widget) prev-selected)
   ;(if (eq (getf input-config :source) 'key-and-mouse) 
    ;   (progn
          (dolist (widget (getf page :widgets))
             (if (point-collide-p 
                   (getf widget :default-rect)
		   (getf mouse-pos :x)
		   (getf mouse-pos :y))
                 (setf selected-widget widget)))
   ;(if (eq (getf input-config :source) 'gamepad)
   ;    (setf selected-widget (get-controller-selected-widget page prev-selected input-config)))
   (setf (getf selected-widget :selected) t)
   (if (not (eql selected-widget prev-selected))
       (progn (widget-callback prev-selected 'deselected)
              (setf (getf prev-selected :selected) nil)))
   (widget-callback selected-widget 'selected)
   selected-widget))

(defun keep-time (target-time)
  (let* ((current-time (sdl2:get-ticks))
         (delta-t (- current-time *prev-time*)))
    (if (> target-time delta-t)
        (sdl2:delay (- target-time delta-t)))
    (setf *prev-time* current-time)
    (setf *delta-t* (if (= delta-t 0) 1 delta-t))))

(defun process-page (gui page)
  (if page (progn
     (let ((flags (get-input-signals)))
      (let ((selected-widget (get-selected-widget page)))
       (mapcar (lambda (flag) 
		 (unless (eq flag 'always)
                  (widget-callback selected-widget flag)))
               flags))
      (mapcar (lambda (flag)
                (widget-callback page flag))
              flags)
      (unless (eql (getf page :render-target) (getf gui :window))
	      (if (getf gui :window)
	          (blit (getf page :render-target) (getf gui :window))))
            ;(process-page page (getf page :current-page))
      (dolist (widget (getf page :widgets))
            (widget-callback widget 'always)
            (if (getf widget :active)
                ;(progn
                  ;(if (and (eq (getf widget :type) 'typing-bar)
              	  ;       (eq (getf widget :input-source) 'gamepad)
          	;	 (not (eq (getm gui :current-page :name) 'gamepad-typing-bar))
          	;	 (not (eq (getm gui :current-page :name) 'gamepad-typing-bar-shift)))
          	      ;(progn
          	;	(setf (getf widget :active) nil)
          	;	(setf (getf widget :selected) nil)
          	;        (set-current-page gui 'gamepad-typing-bar))
                  (widget-callback widget 'active))
            (if (and (getf widget :active)
                     (member 'fleeting-lmb-down flags)
                     (not (getf widget :selected)))
                (widget-callback widget 'click-away))
            (blit (getf widget :surface) (getf page :render-target) :dest-rect (getf widget :default-rect)))))))

(defun free-page (page)
  (loop for widget in (getf page :widgets) collect
       (cond ((eq (getf widget :type) 'button)
              (free-button widget))
             ((eq (getf widget :type) 'typing-bar)
              (free-typing-bar widget))
	     ((eq (getf widget :type) 'label)
	      (free-label widget))
	     ((eq (getf widget :type) 'tab-frame)
	      (free-tab-frame widget)))))

(defun close-application (gui)
  (loop for page in (getf gui :pages) collect
        (free-page page))
  (free-window (getf gui :window)))
      

(defun display-time-strings ()
  (dolist (string (reverse *time-strings*))
    (format t string)))

(defun run-application (gui)
  (reset-events)
  (set-input-flags)
  (clear-window (getf gui :window))
  (process-page gui (getf gui :current-page))
  (flip-window (getf gui :window))
  (if (>= (incf *frames*) 65335)
      (setf *frames* 0))
  (keep-time 7)
  (if *running*
   (run-application gui)))
