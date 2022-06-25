(defparameter *default-button-config* 
  (list :button-up-surface 'default-up-surface
        :button-down-surface 'default-down-surface
        :button-select-up-surface 'default-up-surface
        :button-select-down-surface 'default-down-surface
        :button-up-font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :button-down-font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :button-select-font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :colors (list :button-up-colors (list :base-color #xff998888
                                               :top-border #xffbbbbbb
                                               :bottom-border #xff444444
                                               :text-color #xff000000)
                       :button-down-colors (list :base-color #xff776666
                                                 :top-border #xff444444
                                                 :bottom-border #xffbbbbbb
                                                 :text-color #xff000000)
                       :button-select-up-colors (list :base-color #xff888899
                                                   :top-border #xffbbbbbb
                                                   :bottom-border #xff444444
                                                   :text-color #xff000000)
                        :button-select-down-colors (list :base-color #xff666677
                                                         :top-border #xff444444
                                                         :bottom-border #xffbbbbbb
                                                         :text-color #xff000000))
        :button-up-border-width 4
        :button-down-border-width 4
        :button-select-border-width 4))
