(in-package :spill)
(defparameter *running* t)

(defparameter *fleeting-input* nil)

;(defparameter *shift-hash* (make-hash-table :test 'eql))

(defparameter *stable-input*
  (list :up nil
        :down nil 
	:left nil
        :right nil
        :lmb-down nil
	:prev-mouse-x 0
	:prev-mouse-y 0
        :mouse-x 0 
        :mouse-y 0
        :controller-a nil
        :controller-b nil
        :controller-x nil
        :controller-y nil
        :controller-rb nil
        :controller-lb nil
        :controller-rt nil
        :controller-lt nil
        :controller-start nil
        :keys-down nil))

(defun controller-movement-p ()
  (or (getf *stable-input* :left)
      (getf *stable-input* :right)
      (getf *stable-input* :up)
      (getf *stable-input* :down)))

(defun fleeting-controller-movement-p ()
  (or (getf *fleeting-input* :left)
      (getf *fleeting-input* :right)
      (getf *fleeting-input* :up)
      (getf *fleeting-input* :down)))

(defun get-relative-mouse-pos (widget)
  (list :x (- (getf *stable-input* :mouse-x)
	          (getm widget :default-rect :x))
	:y (- (getf *stable-input* :mouse-y)
	      (getm widget :default-rect :y))))

(defun get-input-signals (&optional (input-config *default-input-config*))
  (let ((flags nil)
        (controller-motion (controller-movement-p)))
   (if (getf *fleeting-input* :lmb-down) (push 'lmb-down flags))
   (if (and (getf *fleeting-input* :mouse-motion) (getf *stable-input* :lmb-down))
       (push 'click-and-drag flags))
   (if (getf *fleeting-input* :lmb-up)
       (push 'click flags))
   (if (getf *stable-input* :lmb-down)
       (push 'hold-mouse flags))
   (if (and (not (getf *fleeting-input* :mouse-motion)) (not (getf *stable-input* :lmb-down)))
       (push 'hover-mouse flags))
   (if (getf *fleeting-input* :left)
       (push 'fleeting-left flags)) 
   (if (getf *fleeting-input* :right)
       (push 'fleeting-right flags))
   (if (getf *fleeting-input* :up)
       (push 'fleeting-up flags))
   (if (getf *fleeting-input* :down)
       (push 'fleeting-down flags)) 
   (if (or (getf *fleeting-input* :lmb-down)
	   (getf *fleeting-input* (getf input-config :controller-click)))
       (push 'fleeting-lmb-down flags))
   (if (getf *fleeting-input* (intern (concatenate 'string (symbol-name (getf input-config :controller-click)) "-UP") :keyword))
       (push 'click flags))
   (if (and controller-motion 
            (getf *stable-input* (getf input-config :controller-click)))
       (push 'click-and-drag flags))
   (if (and (not (getf *stable-input* (getf input-config :controller-click))) 
            (not controller-motion))
       (push 'mouse-hover flags))
   (if (and (getf *stable-input* (getf input-config :controller-click))
            (not controller-motion))
       (push 'hold-mouse flags))
   (if (getf *stable-input* :keys-down)
       (push 'key-down flags))
   (if (getf *fleeting-input* :keys-down)
       (push 'fleeting-key-down flags))
   (if (getf *fleeting-input* :controller-a)
       (push 'fleeting-controller-a-down flags))
   (if (getf *fleeting-input* :controller-b)
       (push 'fleeting-controller-b-down flags))
   (if (getf *fleeting-input* :controller-x)
       (push 'fleeting-controller-x-down flags))
   (if (getf *fleeting-input* :controller-y)
       (push 'fleeting-controller-y-down flags))
   (if (getf *stable-input* :controller-a)
       (push 'controller-a-down flags))
   (if (getf *stable-input* :controller-b)
       (push 'controller-b-down flags))
   (if (getf *stable-input* :controller-x)
       (push 'controller-x-down flags))
   (if (getf *stable-input* :controller-y)
       (push 'controller-y-down flags))
   (push 'always flags)
   (dolist (key (getf *stable-input* :keys-down))
     (if (or (string-equal key "keypad-enter") 
             (string-equal key "return"))
         (push 'enter flags)))
   flags))

(defun stupid-shift-checker (key)
  (cond ((and (alpha-char-p (char key 0))
              (= (length key) 1))
         (string-upcase key))
        ((string-equal key " ")
         key)
        ((= (length key) 1)
         (first (multiple-value-list (gethash 
                                     (cond ((eql key "`") 'backquote)
                                           ((eql key ",") 'comma)
                                           ((eql key ".") 'dot)
                                           (t (intern key)))
                                     *shift-hash*))))
        (t key)))

(defun shift-down-p ()
  (let ((response nil))
    (dolist (key (getf *stable-input* :keys-down))
      (if (or (string-equal key "left shift")
              (string-equal key "right shift"))
          (setf response t)))
    response))

(defun reset-events ()
 (setf *fleeting-input*
       (list 
        :continue-p t
        :up nil
        :down nil
        :left nil
        :right nil
        :lmb-down nil
        :lmb-up nil 
        :mouse-motion nil
	:keys-down nil)))

(defun key-down-p (keys-down key)
  (let ((return-value nil))
    (dolist (current-key keys-down)
      (if (string-equal key current-key) (setf return-value t)))
    return-value))
        
(defun set-key-mouse-input-flags (&optional (input-config *default-input-config*))
  (let ((event nil))
    (setf event (get-event))
    (if (string-equal (getf event :key) "space")
        (setf (getf event :key) " "))
    (setf (getf *stable-input* :prev-mouse-x)
	  (getf *stable-input* :mouse-x))
    (setf (getf *stable-input* :prev-mouse-y)
	  (getf *stable-input* :mouse-y))
    (if (eq (getf input-config :source) 'key-and-mouse)
     (cond ((eq (getf event :type) :keydown)
	    (progn (push (getf event :key) (getf *fleeting-input* :keys-down))
            (cond ((string-equal (getf event :key) "right")
                  (progn (setf (getf *fleeting-input* :right) t)
                         (setf (getf *stable-input* :right) t)))
                 ((string-equal (getf event :key) "left")
                  (progn (setf (getf *fleeting-input* :left) t)
                         (setf (getf *stable-input* :left) t)))
                 ((string-equal (getf event :key) "up")
                  (progn (setf (getf *fleeting-input* :up) t)
                         (setf (getf *stable-input* :up) t)))
                 ((string-equal (getf event :key) "down")
                  (progn (setf (getf *fleeting-input* :down) t)
                         (setf (getf *stable-input* :down) t)))
                 (t
                  (if (not (getf *fleeting-input* :key))
                      (progn (setf (getf *fleeting-input* :key) 
                                   (if (shift-down-p)
                                       (stupid-shift-checker (getf event :key))
                                       (getf event :key)))
                             (if (not (key-down-p (getf *stable-input* :keys-down) (getf event :key)))
                                 (push 
                                   (if (shift-down-p)
                                       (stupid-shift-checker (getf event :key))
                                       (getf event :key))
                                   (getf *stable-input* :keys-down)))))))))
            
          ((eq (getf event :type) :keyup)
           (cond ((string-equal (getf event :key) "right")
                         (setf (getf *stable-input* :right) nil))
                 ((string-equal (getf event :key) "left")
                         (setf (getf *stable-input* :left) nil))
                 ((string-equal (getf event :key) "up")
                         (setf (getf *stable-input* :up) nil))
                 ((string-equal (getf event :key) "down")
                         (setf (getf *stable-input* :down) nil))
                 (t
                  (dolist (key (getf *stable-input* :keys-down))
                    (if (string-equal key (getf event :key))
                        (setf (getf *stable-input* :keys-down) (remove key (getf *stable-input* :keys-down))))))))

          ((eq (getf event :type) :mousebuttondown)
           (cond ((eql (getf event :mouse-button) 1)
                  (progn (setf (getf *fleeting-input* :lmb-down) t)
                         (setf (getf *stable-input* :lmb-down) t)
                         (setf (getf *stable-input* :mouse-x) (getf event :mouse-x))
                         (setf (getf *stable-input* :mouse-y) (getf event :mouse-y))))))
          
          ((eq (getf event :type) :mousebuttonup)
           (cond ((eql (getf event :mouse-button) 1)
                  (progn
                   (setf (getf *stable-input* :lmb-down) nil)
                   (setf (getf *fleeting-input* :lmb-up) t)
                   (setf (getf *stable-input* :mouse-x) (getf event :mouse-x))
                   (setf (getf *stable-input* :mouse-y) (getf event :mouse-y))))))
          
          ((eq (getf event :type) :mousemotion)
           (progn 
            (setf (getf *fleeting-input* :mouse-motion) t)
            (setf (getf *stable-input* :mouse-x) (getf event :mouse-x))
            (setf (getf *stable-input* :mouse-y) (getf event :mouse-y))))
          
          ((eq (getf event :type) :quit)
           (setf *running* nil))
          
          (t
           (setf (getf *fleeting-input* :continue-p) nil)))
     (cond ((eq (getf event :type) :quit)
            (setf *running* nil))
           (t (setf (getf *fleeting-input* :continue-p) nil))))))

(defun set-gamepad-input-flags ()
  (let ((events (get-controller-events)))
    (dolist (event events)
      (cond ((eq (getf event :type) :joystick-motion)
	     ; This is what the conditional was before. I changed it back 
	     ; because it wasn't reading dpad input. Idk if it there was I reason I was doing it that way, but if there's some bug or whatever
	     ; with controller input take a look.
	     ;(and (eq (getf event :type) :joystick-motion) (or (eq (getf event :joystick-axis) :l-v) (eq (getf event :joystick-axis) :l-h)))
             (progn
              (cond ((eq (getf event :joystick-axis) :l-h)
                     (progn
                       (if (> (getf event :joystick-magnitude) 0.7)
                           (progn (setf (getf *fleeting-input* :right) t)
                                  (setf (getf *stable-input* :right) t))
                           (progn (setf (getf *fleeting-input* :right) nil)
                                  (setf (getf *stable-input* :right) nil)))
                       (if (< (getf event :joystick-magnitude) -0.7)
                           (progn (setf (getf *fleeting-input* :left) t)
                                  (setf (getf *stable-input* :left) t))
                           (progn (setf (getf *fleeting-input* :left) nil)
                                  (setf (getf *stable-input* :left) nil)))))
                     ((eq (getf event :joystick-axis) :l-v)
                      (progn
                        (if (> (getf event :joystick-magnitude) 0.7)
                            (progn (setf (getf *fleeting-input* :down) t)
                                   (setf (getf *stable-input* :down) t))
                            (progn (setf (getf *fleeting-input* :down) nil)
                                   (setf (getf *stable-input* :down) nil)))
                        (if (< (getf event :joystick-magnitude) -0.7)
                            (progn (setf (getf *fleeting-input* :up) t)
                                   (setf (getf *stable-input* :up) t))
                            (progn (setf (getf *fleeting-input* :up) nil)
                                   (setf (getf *stable-input* :up) nil)))))
                     
                     ((eq (getf event :joystick-axis) :dpad-h)
                      (progn
                        (if (eql (getf event :joystick-magnitude) 1.0)
                            (progn (setf (getf *fleeting-input* :right) t)
                                   (setf (getf *stable-input* :right) t)
                                   (setf (getf *stable-input* :left) nil)))
                        (if (eql (getf event :joystick-magnitude) -1.0)
                            (progn (setf (getf *fleeting-input* :left) t)
                                   (setf (getf *stable-input* :left) t)
                                   (setf (getf *stable-input* :right) nil)))
                        (if (eql (getf event :joystick-magnitude) 0.0)
                            (progn (setf (getf *stable-input* :right) nil)
                                   (setf (getf *stable-input* :left) nil)))))
                     ((eq (getf event :joystick-axis) :dpad-v)
                      (progn
                        (if (eql (getf event :joystick-magnitude) 1.0)
                            (progn (setf (getf *fleeting-input* :down) t)
                                   (setf (getf *stable-input* :down) t)
                                   (setf (getf *stable-input* :up) nil)))
                        (if (eql (getf event :joystick-magnitude) -1.0)
                            (progn (setf (getf *fleeting-input* :up) t)
                                   (setf (getf *stable-input* :up) t)
                                   (setf (getf *stable-input* :down) nil)))
                        (if (eql (getf event :joystick-magnitude) 0.0)
                            (progn (setf (getf *stable-input* :down) nil)
                                   (setf (getf *stable-input* :up) nil))))))))
            
            ((eq (getf event :type) :button-down)
             (cond ((eq (getf event :button) :a)
                    (progn (setf (getf *fleeting-input* :controller-a) t)
                           (setf (getf *stable-input* :controller-a) t)))
                   ((eq (getf event :button) :b)
                    (progn (setf (getf *fleeting-input* :controller-b) t)
                           (setf (getf *stable-input* :controller-b) t)))
                   ((eq (getf event :button) :x)
                    (progn (setf (getf *fleeting-input* :controller-x) t)
                           (setf (getf *stable-input* :controller-x) t)))
                   ((eq (getf event :button) :y)
                    (progn (setf (getf *fleeting-input* :controller-y) t)
                           (setf (getf *stable-input* :controller-y) t)))
                   ((eq (getf event :button) :r1)
                    (progn (setf (getf *fleeting-input* :controller-rb) t)
                           (setf (getf *stable-input* :controller-rb) t)))
                   ((eq (getf event :button) :r2)
                    (progn (setf (getf *fleeting-input* :controller-rt) t)
                           (setf (getf *stable-input* :controller-rt) t)))
                   ((eq (getf event :button) :l1)
                    (progn (setf (getf *fleeting-input* :controller-lb) t)
                           (setf (getf *stable-input* :controller-lb) t)))
                   ((eq (getf event :button) :l2)
                    (progn (setf (getf *fleeting-input* :controller-lt) t)
                           (setf (getf *stable-input* :controller-lt) t)))
                   ((eq (getf event :button) :start)
                    (progn (setf (getf *fleeting-input* :controller-start) t)
                           (setf (getf *stable-input* :controller-start) t)))))
            
            ((eq (getf event :type) :button-up)
             (cond ((eq (getf event :button) :a)
                    (progn (setf (getf *fleeting-input* :controller-a-up) t)
                           (setf (getf *stable-input* :controller-a) nil)))
                   ((eq (getf event :button) :b)
                    (progn (setf (getf *fleeting-input* :controller-b-up) t)
                           (setf (getf *stable-input* :controller-b) nil)))
                   ((eq (getf event :button) :x)
                    (progn (setf (getf *fleeting-input* :controller-x-up)t )
                           (setf (getf *stable-input* :controller-x) nil)))
                   ((eq (getf event :button) :y)
                    (progn (setf (getf *fleeting-input* :controller-y-up) t)
                           (setf (getf *stable-input* :controller-y) nil)))
                   ((eq (getf event :button) :r1)
                    (progn (setf (getf *fleeting-input* :controller-rb-up) t)
                           (setf (getf *stable-input* :controller-rb) nil)))
                   ((eq (getf event :button) :r2)
                    (progn (setf (getf *fleeting-input* :controller-rt-up) t)
                           (setf (getf *stable-input* :controller-rt) nil)))
                   ((eq (getf event :button) :l1)
                    (progn (setf (getf *fleeting-input* :controller-lb-up) t)
                           (setf (getf *stable-input* :controller-lb) nil)))
                   ((eq (getf event :button) :l2)
                    (progn (setf (getf *fleeting-input* :controller-lt-up) t)
                           (setf (getf *stable-input* :controller-lt) nil)))
                   ((eq (getf event :button) :start)
                    (progn (setf (getf *fleeting-input* :controller-start-up) t)
                           (setf (getf *stable-input* :controller-start) nil)))))))))
                   
                   
(defun set-input-flags (&optional (input-config *default-input-config*))
  (if (eq (getf input-config :source) 'gamepad)
    (set-gamepad-input-flags))
  (loop for i from 0 until (not (getf *fleeting-input* :continue-p)) collect
    (set-key-mouse-input-flags)))


