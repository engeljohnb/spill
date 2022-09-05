#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(in-package :spill)
(defparameter *running* t)
(defparameter *fleeting-input* nil)
(defparameter *hover-mouse-time* 0)
(defparameter *delta-t* 1)

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
        :keys-down nil))

(defun get-mouse-pos ()
  (list :x (getf *stable-input* :mouse-x)
	:y (getf *stable-input* :mouse-y)))

(defun mouse-down-p (mouse-button)
  (cond ((eq mouse-button 'left)
	 (getf *stable-input* :lmb-down))))

(defun get-keys ()
  (let ((keys (getf *stable-input* :keys-down)))
    (if (getf *stable-input* :left)
        (push "left" keys))
    (if (getf *stable-input* :right)
        (push "right" keys))
    (if (getf *stable-input* :up)
        (push "up" keys))
    (if (getf *stable-input* :down)
        (push "down" keys))
    keys))

(defun get-relative-mouse-pos (widget)
  (let ((rect (cond ((member :parent-page-rect widget)
		     (getf widget :parent-page-rect))
		    ((member :default-rect widget)
		     (getf widget :default-rect))
		    ((member :rect widget)
		     (getf widget :rect))
		    (t (progn (print "get-relative-mouse-pose error: invalid widget")
			      (create-rect 0 0 0 0))))))
    (if (and (eq (getf widget :type) 'tab-frame)
             (getf widget :selected)) 
        (progn (print rect)
	       (print (getf *stable-input* :mouse-x))
	       (print (- (getf *stable-input* :mouse-x)
			 (getf rect :x)))))
    (list :x (- (getf *stable-input* :mouse-x)
		(getf rect :x))
  	  :y (- (getf *stable-input* :mouse-y)
		(getf rect :y)))))

(defun get-input-signals (&optional (input-config *default-input-config*))
  (let ((flags nil))
   (if (getf *fleeting-input* :lmb-down) (push 'lmb-down flags))
   (if (and (getf *fleeting-input* :mouse-motion) (getf *stable-input* :lmb-down))
       (push 'click-and-drag flags))
   (if (getf *fleeting-input* :lmb-up)
       (progn (display-time)
	      (fresh-line)
	      (setf *started-blocks* nil)
	      (setf *ended-blocks* nil)
	      (setf *time-keeping-strings* nil)
              (push 'click flags)))
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
   (if (getf *fleeting-input* :lmb-down)
       (push 'fleeting-lmb-down flags))
   (if (getf *stable-input* :keys-down)
       (push 'key-down flags))
   (if (getf *fleeting-input* :keys-down)
       (push 'fleeting-key-down flags))
   (dolist (key (getf *fleeting-input* :keys-down))
     (push (intern (string-upcase key)) flags))
   (push 'always flags)
   (if (member 'hover-mouse flags)
       (incf *hover-mouse-time* *delta-t*)
       (setf *hover-mouse-time* 0))
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
    ;(if (eq (getf input-config :source) 'key-and-mouse)
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
           (t (setf (getf *fleeting-input* :continue-p) nil)))))

(defun set-input-flags (&optional (input-config *default-input-config*))
  (if (eq (getf input-config :source) 'gamepad)
    (set-gamepad-input-flags))
  (loop for i from 0 until (not (getf *fleeting-input* :continue-p)) collect
    (set-key-mouse-input-flags)))
