(defparameter *default-typing-bar-config*
  (list :font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :surface 'default-up-surface
        :button-config *default-button-config*
        :char-spacing 0
        :colors (list :typing-bar-colors (list :base-color #xffccbdbd
                                               :top-border #xff998a8a
                                               :bottom-border #xffffefef)
                      :typing-bar-select-colors (list :base-color #xffbdbdcc
                                                      :top-border #xff8a8a99
                                                      :bottom-border #xffefefff))
        :text-color #xff000000
        :select-text-color #xffffffff
        :highlight-color #xff000000
        :border-width 2))
                                                      
