#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(in-package :spill)

(defun create-rect (x y w h)
  (list :x x :y y :w w :h h))

(defun spillp-convert-rect (rect)
  (sdl2:make-rect (getf rect :x)
                  (getf rect :y)
                  (getf rect :w)
                  (getf rect :h)))

(defun rect-collide-p (rect-1 rect-2)
  (let ((left-1 (getf rect-1 :x))
        (left-2 (getf rect-2 :x))
        (right-1 (+ (getf rect-1 :x) (getf rect-1 :w)))
        (right-2 (+ (getf rect-2 :x) (getf rect-2 :w)))
	(top-1 (getf rect-1 :y))
        (top-2 (getf rect-2 :y))
        (bottom-1 (+ (getf rect-1 :y) (getf rect-1 :h)))
        (bottom-2 (+ (getf rect-2 :y) (getf rect-2 :h))))
    (and (< left-1 right-2)
         (> right-1 left-2)
         (< top-1 bottom-2)
         (> bottom-1 top-2))))
#|
(defun check-rect-collisions (me him)
  (let ((left-me (getf rect-me :x))
        (left-him (getf rect-him :x))
        (right-me (+ (getf rect-me :x) (getf rect-me :w)))
        (right-him (+ (getf rect-him :x) (getf rect-him :w)))
        (top-him (getf rect-him :y))
        (bottom-me (+ (getf rect-me :y) (getf rect-me :h)))
        (bottom-him (+ (getf rect-him :y) (getf rect-him :h))))
    (and (< left-me right-him)
         (> right-me left-him)
         (< top-him bottom-him)
         (> bottom-me top-him)))
|#

(defun point-collide-p (rect x y)
  (let ((left (getf rect :x))
        (right (+ (getf rect :x) (getf rect :w)))
        (top (getf rect :y))
        (bottom (+ (getf rect :y) (getf rect :h))))
    (and (> x left)
         (< x right)
         (> y top)
         (< y bottom))))

(defun fill-surface (surface color)
  (let* ((split-color (break-color color))
         (r (getf split-color :r))
         (g (getf split-color :g))
         (b (getf split-color :b))
         (a (getf split-color :a))
         (prev-color (break-color (sdl2:get-render-draw-color (getf surface :sdl-renderer)))))
   (sdl2:set-render-target (getf surface :sdl-renderer) (getf surface :sdl-texture))
   (sdl2:set-render-draw-color (getf surface :sdl-renderer) r g b a)
   (sdl2:render-clear (getf surface :sdl-renderer))
   (sdl2:set-render-draw-color (getf surface :sdl-renderer)
                               (getf prev-color :r)
                               (getf prev-color :g)
                               (getf prev-color :b)
                               (getf prev-color :a))
   (sdl2:set-render-target (getf surface :sdl-renderer) nil)
   surface))
                               
(defun create-surface (window x y w h)
  (let ((sdl-texture (sdl2:create-texture (getf window :sdl-renderer) :argb8888 :target w h)))
    (sdl2:set-texture-blend-mode sdl-texture :blend)
  (list :sdl-texture sdl-texture 
        :sdl-renderer (getf window :sdl-renderer)
        :rect (create-rect x y w h)
        :window window)))

(defun blit-to-surface (source dest source-rect dest-rect)
  (let ((sdl-dest-rect
          (if (not dest-rect) 
              (spillp-convert-rect (getf source :rect))
              (spillp-convert-rect dest-rect)))
        (sdl-source-rect
          (if (not source-rect) 
              (sdl2:make-rect 
                          0
                          0
                          (getm source :rect :w)
                          (getm source :rect :h))
              (spillp-convert-rect source-rect))))
  (sdl2:set-render-target (getf dest :sdl-renderer) (getf dest :sdl-texture))
  (sdl2:render-copy (getf dest :sdl-renderer) 
                    (getf source :sdl-texture)
                    :source-rect sdl-source-rect
                    :dest-rect sdl-dest-rect)
  (sdl2:set-render-target (getf dest :sdl-renderer) nil)))
  
(defun blit-to-window (source dest source-rect dest-rect)
  (let ((sdl-dest-rect
           (if (not dest-rect) 
               (spillp-convert-rect (getf source :rect))
               (spillp-convert-rect dest-rect)))
        (sdl-source-rect
           (if (not source-rect) 
               (sdl2:make-rect 
                        0 
                        0 
                        (getm source :rect :w)
                        (getm source :rect :h))
               (spillp-convert-rect source-rect))))
  (sdl2:render-copy (getf dest :sdl-renderer) 
                    (getf source :sdl-texture) 
                    :source-rect sdl-source-rect 
                    :dest-rect sdl-dest-rect)))
                          
(defun blit (source dest &key (source-rect nil) (dest-rect nil))
  (cond ((member :sdl-window dest)
         (blit-to-window source dest source-rect dest-rect))
        ((member :sdl-texture dest)
         (blit-to-surface source dest source-rect dest-rect))
	((member :surface dest)
	 (if (member :surface source)
	     (blit-to-surface (getf source :surface) (getf dest :surface))
	     (blit-to-surface source :surface (getf dest :surface))))
        (t (progn (format t "blit error: invalid surface: ~
