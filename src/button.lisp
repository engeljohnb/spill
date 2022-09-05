#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(in-package :spill)

(defun create-button (window 
                            width 
                            height 
                            name 
                            &optional 
                            (animation nil)
                            (config *default-button-config*))
  (let* ((button-config config)
         (up-surface (funcall (getf button-config  :button-up-surface) 
                              window 
                              width 
                              height 
                              name 
                              (getf button-config :button-up-font) 
                              (getm button-config :colors :button-up-colors)
                              (getf button-config :button-up-border-width)))
         (down-surface (funcall 
                         (getf button-config :button-down-surface) 
                               window 
                               width 
                               height 
                               name 
                               (getf button-config :button-down-font) 
                               (getm button-config :colors :button-down-colors)
                               (getf button-config :button-down-border-width)))
         (select-up-surface (funcall
                            (getf button-config :button-select-up-surface)
                                window
                                width
                                height
                                name
                                (getf button-config :button-up-font)
                                (getm button-config :colors :button-select-up-colors)
                                (getf button-config :button-select-border-width)))
         (select-down-surface (funcall
                                (getf button-config :button-select-down-surface)
                                 window
                                 width
                                 height
                                 name
                                 (getf button-config :button-down-font)
                                 (getm button-config :colors :button-select-down-colors)
                                 (getf button-config :button-select-border-width)))
         (button (list 
                      :type 'button
                      :name name
                      :up-surface up-surface
                      :down-surface down-surface
                      :select-up-surface select-up-surface
                      :select-down-surface select-down-surface
                      :selected nil
                      :callbacks nil
		      :currently-down nil
		      :frames-down 0
                      :callback-data nil
                      :animation animation
                      :surface up-surface
                      :default-rect (getf up-surface :rect)
		      :freed nil)))
    (setf (getf button :callbacks) (list 
                                    (cons 'hold-mouse #'(lambda (button) 
							  (setf (getf button :currently-down) t)))
                                    ;(cons 'click #'(lambda (button) (setf (getf button :surface) (getf button :select-up-surface))))
                                    (cons 'selected #'(lambda (button) (setf (getf button :surface) (getf button :select-up-surface))))
                                    (cons 'deselected #'(lambda (button) (setf (getf button :surface) (getf button :up-surface))))
	  			    (cons 'always #'(lambda (button)
						      (if (getf button :currently-down)
							  (progn
							    (unless (or (< (incf (getf button :frames-down)) 5)
								        (mouse-down-p 'left))
							            (setf (getf button :currently-down) nil))
							    (setf (getf button :surface) (getf button :select-down-surface))))))))
    (setf (getf button :callback-data) (list 
                                        (cons 'hold-mouse button)
                                        ;(cons 'click button)
                                        (cons 'selected button)
                                        (cons 'deselected button)
					(cons 'always button)))
    button))

(defun name->button (name list-of-buttons)
  (let ((desired-button nil))
    (dolist (button list-of-buttons)
     (if (string-equal name (getf button :name))
         (setf desired-button button)))
    desired-button))

(defun get-longest-segment (segments)
  (let ((current-longest (first segments)))
    (dolist (segment segments)
      (if (>= (length segment) (length current-longest))
	  (setf current-longest segment)))
    current-longest))

(defun get-text-height (segments font)
  (let ((current-height 0))
    (dolist (segment segments)
      (if (> (length segment) 0)
	  (incf current-height (getf (get-text-size font segment) :h))
	  (incf current-height (getf (get-text-size font "0") :h))))
    current-height))


(defun create-info-box (window text &key (config *default-info-box-config*) (font-size 11))
  (let* ((info-box (list
		    :type 'info-box
		    :surface nil
		    :font (open-font (getf config :font) font-size)
		    :border-width (getf config :border-width)
		    :default-rect nil))
	 (segments (split-text text))
	 (longest-segment (get-longest-segment segments))
	 (number-of-lines (length segments))
	 (text-width (getf (get-text-size (getf info-box :font) longest-segment) :w))
	 (text-height (get-text-height segments (getf info-box :font)))
	 (current-y (getf config :border-width)))
    (setf (getf info-box :surface) (create-surface window 
						   0 0 
						   (+ text-width (getf info-box :border-width) 2)
						   (+ text-height (getf info-box :border-width) 2)))
    (fill-surface (getf info-box :surface) (getf config :base-color))
    (draw-rect (getf info-box :surface)
	       (getf config :border-color)
	       (create-rect 0 0 
			  (getm info-box :surface :rect :w) 
			  (getf info-box :border-width)))
    (draw-rect (getf info-box :surface)
	       (getf config :border-color)
	       (create-rect (- (getm info-box :surface :rect :w) (getf info-box :border-width))
			  0
			  (getf info-box :border-width)
			  (getm info-box :surface :rect :h)))
    (draw-rect (getf info-box :surface)
	       (getf config :border-color)
	       (create-rect 0 0
			  (getf info-box :border-width)
			  (getm info-box :surface :rect :h)))
    (draw-rect (getf info-box :surface)
	       (getf config :border-color)
	       (create-rect 0
			  (- (getm info-box :surface :rect :h) (getf info-box :border-width))
			  (getm info-box :surface :rect :w)
			  (getf info-box :border-width)))
    (setf (getf info-box :default-rect) (getm info-box :surface :rect))
    (dolist (segment segments)
      (if (> (length segment) 0)
	  (progn
            (draw-text (getf info-box :surface)
	  	       (getf info-box :font)
		       segment
		       :x (+ (getf info-box :border-width))
		       :y current-y)
            (incf current-y (getf (get-text-size (getf info-box :font) segment) :h)))
	  (incf current-y (getf (get-text-size (getf info-box :font) "0") :h))))
    info-box))


(defun create-label (text
		      window
		      width
		      height
		      &key (label-config *default-label-config*)
		           (label-update-callback nil)
			   (label-update-data nil))
  (let ((label (list
		 :type 'label
		 :surface (funcall (getf label-config :surface)
				   window
				   width
				   height
				   nil 
				   (getf label-config :font)
				   (getf label-config :colors)
				   (getf label-config :border-width))
		 :blank-surface
		 :font (open-font (getf label-config :font) (round (* height 0.5)))
		 :default-rect (create-rect 0 0 width height)
		 :string text 
		 :label-update-callback label-update-callback
		 :label-update-data label-update-data
		 :prev-datum nil 
		 :callbacks nil
		 :callback-data 
		 :border-width (getf label-config :border-width)
		 :selected nil)))
    ; These should already be set, but for some reason they aren't set by the statement above
    (setf (getf label :string) text)
    (setf (getf label :font) (open-font (getf label-config :font) (round (* height 0.5))))
    (setf (getf label :label-update-callback) label-update-callback)
    (setf (getf label :label-update-data) label-update-data)
    (setf (getf label :blank-surface) (create-surface window 0 0 width height))
    (setf (getf label :default-rect) (create-rect 0 0 width height))
    (blit (getf label :surface) (getf label :blank-surface))
    (draw-text (getf label :surface)
	       (getf label :font)
	       (getf label :string)
	       :x (+ 1 (getf label :border-width)))
    (blit (getf label :surface)
	  (getf label :blank-surface))
    (setf (getf label :callbacks)
	  (list (cons 'always (lambda (label)
				(let* ((new-datum (funcall (getf label :label-update-callback) (getf label :label-update-data)))
				       (string (format nil "~A" new-datum))
				       (size (get-text-size (getf label :font) string)))
				   (if (not (eql new-datum (getf label :prev-datum)))
				       (progn (blit (getf label :blank-surface) (getf label :surface))
				   	      (draw-text (getf label :surface) 
						         (getf label :font) 
							 string
						         :x (- (getm label :surface :rect :w) (getf size :w) (getf label :border-width)))))
				   (setf (getf label :prev-datum) new-datum))))))
    (setf (getf label :callback-data)
	  (list (cons 'always label)))
    label))

(defun free-button (button)
  (unless (getf button :freed)
          (free-surface (getf button :up-surface))
          (free-surface (getf button :down-surface))
          (free-surface (getf button :select-up-surface))
          (free-surface (getf button :select-down-surface))
	  (setf (getf button :freed) t)))
          ;Don't forget to free the animations!!

(defun free-label (label)
  (close-font (getf label :font))
  (free-surface (getf label :surface))
  (free-surface (getf label :blank-surface)))
