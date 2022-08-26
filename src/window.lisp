#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

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
    (setf (gethash '|5| shift-hash) "
