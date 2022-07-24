(in-package :spill)

(defun getm-iter (item &rest members)
  `(getf ,item ,(first members)))

(defmacro getm (item &rest members)
  (let ((result item))
    (car 
     (last
      (loop for mem in members
       collect
       (setf result (getm-iter result mem)))))))

(defun break-color (color)
  (let ((a (ash (logand color #xff000000) -24))
        (r (ash (logand color #x00ff0000) -16))
        (g (ash (logand color #x0000ff00) -8))
        (b (logand color #x000000ff)))
    (list :a a :r r :g g :b b)))

(defun empty-string-p (string)
  (or (not string) 
      (not (> (length string) 0))))

(defun get-center (rect)
  (cons (+ (getf rect :x) (round (/ (getf rect :w) 2))) (+ (getf rect :y) (round (/ (getf rect :h) 2)))))

(defun get-distance (rect1 rect2 &optional (round-p t))
  (let* ((p1 (get-center rect1))
         (p2 (get-center rect2))
         (x1 (car p1))
         (y1 (cdr p1))
         (x2 (car p2))
         (y2 (cdr p2)))
    (if round-p
        (round (sqrt (abs (+ (expt (- x2 x1) 2)  (expt (- y2 y1) 2)))))
        (abs (sqrt (abs (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))))

(defun get-rect-in-rect (inner-rect outer-rect)
  "Get a rect's coordinates relative to a bigger rect containing it"
  (list :x (- (getf inner-rect :x) (getf outer-rect :x))
	:y (- (getf inner-rect :y) (getf outer-rect :y))
	:w (getf inner-rect :w)
	:h (getf inner-rect :h)))
            
(defun middle (number-1 number-2)
  (- (round (/ (max number-1 number-2) 2)) (round (/ (min number-1 number-2) 2))))

(defun split-text (text &key (dividing-character #\newline))
  (let ((segments nil)
	(segment-start 0)
	(segment-end 0))
    (map 'string 
	 #'(lambda (char)
	      (if (or (eql char dividing-character)
		      (= segment-end (- (length text) 1)))
		  (progn 
		    (push (subseq text segment-start segment-end) segments)
		    (setf segment-start (+ segment-end 1))))
	      (incf segment-end)
	      char)
	 text)
    (setf (first segments)
	  (concatenate 'string
		       (first segments)
		       (string (last-char text))))
    (reverse segments)))
