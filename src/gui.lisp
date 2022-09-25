#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

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
     (progn 
       (let ((page (get-page gui page-name)))
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
      (unless (eql (getf page :render-target) (getf gui :window))
	      (if (getf gui :window)
	          (blit (getf page :render-target) (getf gui :window))))
      (mapcar (lambda (flag)
                (widget-callback page flag))
	      flags)
       (mapcar (lambda (flag) 
		 (unless (eq flag 'always)
                  (widget-callback selected-widget flag)))
               flags)
      (if (getf page :widgets)
        (dolist (widget (getf page :widgets))
              (if (getf widget :active)
                    (widget-callback widget 'active))
              (if (and (getf widget :active)
                       (member 'fleeting-lmb-down flags)
                       (not (getf widget :selected)))
                  (widget-callback widget 'click-away))
              (widget-callback widget 'always)
              (blit (getf widget :surface) (getf page :render-target) :dest-rect (getf widget :default-rect)))))))))

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
