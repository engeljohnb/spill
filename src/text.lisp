#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(in-package :spill)

(defparameter *controller-typing-page* nil)
(defparameter *ctp-init* nil)

(defparameter *key-row-0* (list "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "<-"))
(defparameter *key-row-1* (list "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\"))
(defparameter *key-row-2* (list "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "return"))
(defparameter *key-row-3* (list "z" "x" "c" "v" "b" "n" "m" "," "." "/" "shift"))
(defparameter *key-row-4* (list " "))

(defparameter *shift-key-row-0* (list "~" "!" "@" "#" "#" "
