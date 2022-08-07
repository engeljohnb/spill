(in-package :spill)

;(require 'sdl2)
;(require 'sdl2-ttf)
;(require 'cl-gamepad)
;(rename-package 'org.shirakumo.fraf.gamepad 'gamepad)
;(rename-package 'cl-gamepad 'gamepad)

(defparameter *started-blocks* nil)
(defparameter *ended-blocks* nil)
(defparameter *time-keeping-strings* nil)
(defparameter *shift-hash* (make-hash-table :test 'eql))
(defparameter *frames* 0)

(defun init-shift-checker (shift-checker)
  (let ((shift-hash shift-checker))
    (setf (gethash 'backquote shift-hash) "~")
    (setf (gethash '|1| shift-hash) "!")
    (setf (gethash '|2| shift-hash) "@")
    (setf (gethash '|3| shift-hash) "#")
    (setf (gethash '|4| shift-hash) "$")
    (setf (gethash '|5| shift-hash) "%")
    (setf (gethash '|6| shift-hash) "^")
    (setf (gethash '|7| shift-hash) "&")
    (setf (gethash '|8| shift-hash) "*")
    (setf (gethash '|9| shift-hash) "(")
    (setf (gethash '- shift-hash) "_")
    (setf (gethash '= shift-hash) "+")
    (setf (gethash '[ shift-hash) "{")
    (setf (gethash '] shift-hash) "}")
    (setf (gethash '\\ shift-hash) "|")
    (setf (gethash 'comma shift-hash) "<")
    (setf (gethash 'dot shift-hash) ">")
    (setf (gethash '/ shift-hash) "?")))


(defun start-time (block-name)
  (unless (eq block-name (car (assoc block-name *started-blocks*)))
          (push (cons block-name (sdl2:get-ticks)) *started-blocks*)))

(defun end-time (block-name)
  (format t "~A ~A ~%" block-name (if (assoc block-name *started-blocks*) (- (sdl2:get-ticks) (cdr (assoc block-name *started-blocks*))) 0))
  (setf *started-blocks* (remove (assoc block-name *started-blocks*) *started-blocks*)))

#|(defun end-time (block-name)
  (let* ((current-time (sdl2:get-ticks))
	 (time-block (assoc block-name *started-blocks*))
	 (delta-t (if time-block (- current-time (cdr time-block)) nil)))
    (unless (member block-name *ended-blocks*)
            (progn (push (format nil "~A ~A ~%" (car time-block) (cdr time-block)) *time-keeping-strings*)
		   (push block-name *ended-blocks*)))))
|#
(defun display-time ()
  nil)

(defun init ()
  (sdl2:init :everything)
  (sdl2-ttf:init)
  (org.shirakumo.fraf.gamepad:init)
  (init-shift-checker *shift-hash*))


(defun create-window (title x y w h &key (fullscreen nil))
  (let ((flags nil))
    (if (eql fullscreen t)
        (push ':fullscreen-desktop flags))
    (let* ((window (sdl2:create-window :title title 
                        :x x 
                        :y y 
                        :w w
                        :h h
                        :flags flags))
           (renderer (sdl2:create-renderer window -1 '(:accelerated))))
      (sdl2:set-render-draw-blend-mode renderer :blend)
      (if fullscreen
	  (progn
	    (setf w (first (multiple-value-list (sdl2:get-window-size window))))
	    (setf h (second (multiple-value-list (sdl2:get-window-size window))))))
      (list :sdl-window window :sdl-renderer renderer :width w :height h :rect (list :x 0 :y 0 :w w :h h)))))

(defun set-window-bg (window color)
  (let ((split-color (break-color color)))
    (sdl2:set-render-draw-color (getf window :sdl-renderer)
                                (getf split-color :r)
                                (getf split-color :g)
                                (getf split-color :b)
                                (getf split-color :a))))

(defun clear-window (window)
  (if (member :sdl-window window)
      (sdl2:render-clear (getf window :sdl-renderer))))

(defun flip-window (window)
   (if (member :sdl-window window)
       (sdl2:render-present (getf window :sdl-renderer))))

(defun get-event ()
  (let ((event (list :type :idle
                           :key nil
                           :mouse-x nil
                           :mousey-y nil
                           :mouse-button nil
                           :quit nil)))
    (sdl2:with-sdl-event (sdl-event)
      (sdl2:next-event sdl-event)
       (let ((event-type (sdl2:get-event-type sdl-event)))
        (setf (getf event :type) event-type)
        (cond ((eq event-type :keydown)
               (setf (getf event :key) 
                     (string-downcase (sdl2:get-key-name
                      (sdl2:get-key-from-scancode
                       (sdl2:scancode
                        ; I have no idea what this does but it works
                        (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :key :keysym)))))))
              ((eq event-type :keyup)
               (setf (getf event :key)
                     (string-downcase (sdl2:get-key-name
                      (sdl2:get-key-from-scancode
                       (sdl2:scancode
                        (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :key :keysym)))))))
              ((eq event-type :mousemotion)
               (setf (getf event :mouse-x) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :motion :x))
               (setf (getf event :mouse-y) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :motion :y)))
              ((eq event-type :mousebuttondown)
               (setf (getf event :mouse-x) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :button :x))
               (setf (getf event :mouse-y) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :button :y))
               (setf (getf event :mouse-button)
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :button :button)))
              ((eq event-type :mousebuttonup)
               (setf (getf event :mouse-button) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :button :button))
               (setf (getf event :mouse-y) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :motion :y))
               (setf (getf event :mouse-x) 
                     (sdl2::c-ref sdl-event sdl2-ffi:sdl-event :button :x)))
              ((eq event-type :quit)
               (setf (getf event :quit) t)))))
    event))

;(defun get-events ()
;  (org.shirakumo.fraf.gamepad:poll-devices)
;  (dolist (device (org.shirakumo.fraf.gamepad:list-devices))
;    (org.shirakumo.fraf.gamepad:poll-events device
;      (lambda (event) (print event)))))

;(defun get-controller-events ()
;  (org.shirakumo.fraf.gamepad:poll-devices)
; ; This is the only way I could get it to work
;  (if (not (org.shirakumo.fraf.gamepad:list-devices))
;      (progn (org.shirakumo.fraf.gamepad:shutdown)
;             (org.shirakumo.fraf.gamepad:init)
;             (org.shirakumo.fraf.gamepad:poll-devices)))
;  (let ((events nil))
;   (dolist (device (org.shirakumo.fraf.gamepad:list-devices))
;      (org.shirakumo.fraf.gamepad:poll-events device
;        (lambda (device-event)
;          (let ((event (list :type :idle
;                            :button nil
;                            :joystick-axis nil
;                            :joystick-magnitude nil
;                            :controller nil)))
;            (case (etypecase device-event
;                    (org.shirakumo.fraf.gamepad:button-down
;                      (progn 
;                       (setf (getf event :type) :button-down)
;                       (setf (getf event :button) (org.shirakumo.fraf.gamepad:event-label device-event))))
;                    (org.shirakumo.fraf.gamepad:button-up
;                      (progn 
;                        (setf (getf event :type) :button-up)
;                        (setf (getf event :button) (org.shirakumo.fraf.gamepad:event-label device-event))))
;                    (org.shirakumo.fraf.gamepad:axis-move
;                      (progn
;                       (setf (getf event :type) :joystick-motion)
;                       (setf (getf event :joystick-axis) (org.shirakumo.fraf.gamepad:event-label device-event))
;                       (setf (getf event :joystick-magnitude) (org.shirakumo.fraf.gamepad:event-value device-event))))))
;            (setf (getf event :controller) device)
;            (push event events)))))
;     events))

(defun open-window (window)
  (sdl2:show-window (getf window :sdl-window)))

(defun free-window (window)
  (sdl2:destroy-window (getf window :sdl-window))
  (sdl2:destroy-renderer (getf window :sdl-renderer)))
