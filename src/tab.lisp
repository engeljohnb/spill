#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(in-package :spill)

(defun default-tab-surface (window name x y width height font-path border-width direction config-colors)
  (let ((main-surface (create-surface window x y width height))
	(border-surface (create-surface window 0 
					       0 (- width (* 2 border-width)) 
					       (if (eq direction 'up)
						   (- height border-width)
						   (- height (* 2 border-width)))))
	(font (open-font font-path (round (* height 0.5)))))
    (fill-surface main-surface (getf config-colors :border-color))
    (fill-surface border-surface (getf config-colors :base-color))
    (blit border-surface main-surface :dest-rect (create-rect border-width border-width (getm border-surface :rect :w) (getm border-surface :rect :h)))
    (draw-text main-surface font name :color (getf config-colors :text-color))
    (free-surface border-surface)
    (close-font font)
    main-surface))

(defun add-tab (tab-frame name &key (config *default-tab-config*))
  (let ((tab (list :name (intern (string-upcase name))
		   :type 'tab
		   :surface nil
		   :selected nil
		   :active nil
		   :tab-width (getf tab-frame :tab-width)
		   :tab-height (getf tab-frame :tab-height)
		   :tab-frame-rect (copy-list (getf tab-frame :default-rect))
		   :up-surface (default-tab-surface (getf tab-frame :window)
						       name
						       (* (getf tab-frame :tab-width) (length (getf tab-frame :tabs)))
						       0
						       (getf tab-frame :tab-width)
						       (getf tab-frame :tab-height)
						       (getf config :font)
						       (getf config :border-width)
						       'up
						       (getm config :colors :up-surface-colors))
		   :down-surface (default-tab-surface (getf tab-frame :window)
						      name
						      (* (getf tab-frame :tab-width) (length (getf tab-frame :tabs)))
						      0
						      (getf tab-frame :tab-width)
						      (getf tab-frame :tab-height)
						      (getf config :font)
						      (getf config :border-width)
						      'down
						      (getm config :colors :down-surface-colors))
		   :pages nil
		   :default-rect nil
		   :current-page nil)))
    (decf (getm tab :up-surface :rect :x) (* (length (getf tab-frame :tabs)) (getf tab-frame :border-width)))
    (decf (getm tab :down-surface :rect :x) (* (length (getf tab-frame :tabs)) (getf tab-frame :border-width)))
    (incf (getm tab :tab-frame-rect :y) (+ (getf tab-frame :tab-height) (getf tab-frame :border-width)))
    (incf (getm tab :tab-frame-rect :x) (getf tab-frame :border-width))
    (decf (getm tab :tab-frame-rect :h) (+ (* 2 (getf tab-frame :border-width)) (getf tab-frame :tab-height)))
    (decf (getm tab :tab-frame-rect :w) (* 2 (getf tab-frame :border-width)))
    (if (= (length (getf tab-frame :tabs)) 0)
	(progn (setf (getf tab :surface) (getf tab :up-surface))
	       (setf (getf tab :active) t))
	(setf (getf tab :surface) (getf tab :down-surface)))
    (setf (getf tab :default-rect) (getm tab :surface :rect))
    (add-page tab 'default :render-target (getf tab-frame :render-target))
    (setf (getf tab :current-page) (get-page tab 'default))
    (push tab (getf tab-frame :tabs))))

(defun get-tab (tab-frame tab-name)
  (let ((tab nil))
    (dolist (current-tab (getf tab-frame :tabs))
      (if (eq tab-name (getf current-tab :name))
	  (setf tab current-tab)))
    tab))

(defun tab-frame-get-page (tab-frame tab-name page-name)
  (get-page (get-tab tab-frame tab-name) page-name))

(defun create-tab-frame-surface (window x y width height tab-width tab-height border-width config-colors)
  (let ((main-surface (create-surface window x y width height))
	(border-surface (create-surface window border-width border-width (- width (* 2 border-width)) (- height (* 2 border-width))))
	(tab-area-surface (create-surface window x y width tab-height))
	(tab-area-border-surface (create-surface window border-width border-width (- width (* 2 border-width)) (- tab-height (* 2 border-width)))))
    (fill-surface main-surface (getf config-colors :border-color))
    (fill-surface border-surface (getf config-colors :base-color))
    (fill-surface tab-area-surface (getf config-colors :border-color))
    (fill-surface tab-area-border-surface (getf config-colors :tab-area-color))
    (blit tab-area-border-surface tab-area-surface)
    (blit border-surface main-surface)
    (blit tab-area-surface main-surface)
    (free-surface border-surface)
    (free-surface tab-area-surface)
    (free-surface tab-area-border-surface)
    main-surface))

(defun get-selected-tab (tab-frame &key (input-config *default-input-config*))
  (let ((selected-tab nil)
	(mouse-pos (get-relative-mouse-pos tab-frame)))
   (if (eq (getf input-config :source) 'key-and-mouse)
    (progn
      (dolist (tab (getf tab-frame :tabs))
	  (if (point-collide-p
	        (getf tab :default-rect)
	        (getf mouse-pos :x)
		(getf mouse-pos :y))
	      (setf selected-tab tab)))))
   selected-tab))

(defun process-tab-frame (tab-frame)
  (blit (getf tab-frame :blank-surface) (getf tab-frame :surface))
  (let ((selected-tab (get-selected-tab tab-frame))
	(flags (get-input-signals)))
   (if selected-tab (setf (getf selected-tab :selected) t))
   (dolist (tab (getf tab-frame :tabs))
     (if (and (member 'click flags :test #'eq) selected-tab)
         (progn
	   (setf (getf tab :selected) nil)
	   (setf (getf tab :active) nil)
	   (setf (getf tab :surface) (getf tab :down-surface))
	   (setf (getf selected-tab :active) t)
	   (setf (getf selected-tab :selected) t)
	   (setf (getf selected-tab :surface) (getf selected-tab :up-surface))
           (blit (getf tab-frame :blank-render-target) (getf tab-frame :render-target))))
     (if (getf tab :active)
         (process-page tab (getf tab :current-page)))
     (blit (getf tab-frame :render-target) (getf tab-frame :surface))
     (blit (getf tab :surface) (getf tab-frame :surface)))))

(defun create-tab-frame (window page-width page-height tab-width tab-height &key (config *default-tab-frame-config*) (x 0) (y 0))
  (let ((tab-frame (list :window window
			 :type 'tab-frame
		         :surface nil
			 :blank-surface nil
			 :render-target nil
			 :blank-render-target nil
		         :tabs nil
			 :current-tab nil
			 :tab-width tab-width
			 :tab-height tab-height
			 :border-width (getf config :border-width)
			 :parent-page-rect nil
		         :default-rect (create-rect x
		              		            y
						    page-width
						    page-height)
		         :callbacks nil
		         :callback-data nil)))
    (setf (getf tab-frame :surface) (create-tab-frame-surface window
							      x y
					                     (getm tab-frame :default-rect :w)
					                     (getm tab-frame :default-rect :h)
							     (getf tab-frame :tab-width)
							     (getf tab-frame :tab-height)
					                     (getf config :border-width)
					                     (getm config :colors)))
    (setf (getf tab-frame :blank-surface) (create-tab-frame-surface window
							      x y 
					                     (getm tab-frame :default-rect :w)
					                     (getm tab-frame :default-rect :h)
							     (getf tab-frame :tab-width)
							     (getf tab-frame :tab-height)
					                     (getf config :border-width)
					                     (getm config :colors)))
    (setf (getf tab-frame :render-target) (create-surface window
								 (getf tab-frame :border-width)
								 (+ (getf tab-frame :tab-height) (getf tab-frame :border-width))
								 (- page-width (* 2 (getf tab-frame :border-width)))
								 (- page-height (* 2 (getf tab-frame :border-width)))))
   (setf (getf tab-frame :blank-render-target) (create-surface window
								 0
								 0
								 (- page-width (* 2 (getf tab-frame :border-width)))
								 (- page-height (* 2 (getf tab-frame :border-width)))))
    (fill-surface (getf tab-frame :render-target) (getm config :colors :base-color))
    (fill-surface (getf tab-frame :blank-render-target) (getm config :colors :base-color))
    (setf (getf tab-frame :callbacks) (list (cons 'always #'process-tab-frame)
					    (cons 'page-enter #'process-tab-frame)))
    (setf (getf tab-frame :callback-data) (list (cons 'always tab-frame)
						(cons 'page-enter tab-frame)))
    tab-frame))

(defun free-tab (tab)
  (free-surface (getf tab :up-surface))
  (free-surface (getf tab :down-surface))
  (dolist (page (getf tab :pages))
    ; free-page isn't defined until later because this file is loaded before gui.lisp is
    ; Yes I know this is bad shush
    (if page (funcall 'free-page page))))

(defun free-tab-frame (tab-frame)
  (free-surface (getf tab-frame :blank-surface))
  (free-surface (getf tab-frame :render-target))
  (free-surface (getf tab-frame :blank-render-target))
  (if (getf tab-frame :tabs)
      (dolist (tab (getf tab-frame :tabs))
      (free-tab tab))))
