(defparameter *default-tab-frame-config*
  (list :font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
	:colors (list :base-color #xff88778f
		      :tab-area-color #xff55445e
		      :border-color #xff222222)
	:border-width 2))

(defparameter *default-tab-config*
  (list :up-surface 'default-tab-surface
	:down-surface 'default-tab-surface
	:border-width 2
  	:font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
	:colors (list :up-surface-colors (list :base-color #xff88778f
					       :border-color #xff222222
					       :text-color #xff000000)
		      :down-surface-colors (list :base-color #xff55445e
						 :border-color #xff222222
						 :text-color #xff000000))))
