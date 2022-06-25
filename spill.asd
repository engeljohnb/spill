(defsystem :spill
  :version "0.01"
  :depends-on (:sdl2
  	       :sdl2-ttf
	       :cl-gamepad)
  :pathname "src"
  :serial t
  :components 	((:file "package")
		 (:file "config")
		 (:file "utils")
		 (:file "window")
		 (:file "surface")
		 (:file "event")
		 (:file "text")
		 (:file "button")
		 (:file "gui")))
