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

(defun free-button (button)
  (unless (getf button :freed)
          (free-surface (getf button :up-surface))
          (free-surface (getf button :down-surface))
          (free-surface (getf button :select-up-surface))
          (free-surface (getf button :select-down-surface))
	  (setf (getf button :freed) t)))
          ;Don't forget to free the animations!!
