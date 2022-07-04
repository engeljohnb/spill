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
                      :callback-data nil
                      :animation animation
                      :surface up-surface
                      :default-rect (getf up-surface :rect)
		      :freed nil)))
    (setf (getf button :callbacks) (list 
                                    (cons 'hold-mouse #'(lambda (button) (setf (getf button :surface) (getf button :select-down-surface))))
                                    (cons 'click #'(lambda (button) (setf (getf button :surface) (getf button :select-up-surface))))
                                    (cons 'selected #'(lambda (button) (setf (getf button :surface) (getf button :select-up-surface))))
                                    (cons 'deselected #'(lambda (button) (setf (getf button :surface) (getf button :up-surface))))))
    (setf (getf button :callback-data) (list 
                                        (cons 'hold-mouse button)
                                        (cons 'click button)
                                        (cons 'selected button)
                                        (cons 'deselected button))) 
    button))

(defun name->button (name list-of-buttons)
  (let ((desired-button nil))
    (dolist (button list-of-buttons)
     (if (string-equal name (getf button :name))
         (setf desired-button button)))
    desired-button))

(defun create-label (text
		     window
		      width
		      height
		      &key (label-config *default-label-config*)
		           (string-datum nil))
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
		 :string-datum string-datum
		 :prev-string-datum string-datum
		 :callbacks nil
		 :callback-data 
		 :border-width (getf label-config :border-width)
		 :selected nil)))
    ; These should already be set, but for some reason they aren't set by the statement above
    (setf (getf label :string) text)
    (setf (getf label :font) (open-font (getf label-config :font) (round (* height 0.5))))
    (setf (getf label :string-datum) string-datum)
    (setf (getf label :blank-surface) (create-surface window 0 0 width height))
    (setf (getf label :prev-string-datum) string-datum)
    (setf (Getf label :default-rect) (create-rect 0 0 width height))
    (blit (getf label :surface) (getf label :blank-surface))
    (draw-text (getf label :surface)
	       (getf label :font)
	       (format nil (getf label :string) (getf label :string-datum))
	       :x (+ 1 (getf label :border-width)))
    (setf (getf label :callbacks)
	  (list (cons 'always (lambda (label)
				(if (not (eql (getf label :string-datum) (getf label :prev-string-datum)))
				    (progn (blit (getf label :blank-surface) (getf label :surface))
					   (draw-text (getf label :surface) 
						      (getf label :font) 
						      (format nil (getf label :string) (getf label :string-datum))
						      :x (+ 1 (getf label :border-width)))))
				(setf (getf label :prev-string-datum) (getf label :string-datum))))))
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
  (free-surface (getf label :surface))
  (free-surface (getf label :blank-surface)))


