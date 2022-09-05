#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(defparameter *default-button-config* 
  (list :button-up-surface 'default-up-surface
        :button-down-surface 'default-down-surface
        :button-select-up-surface 'default-up-surface
        :button-select-down-surface 'default-down-surface
        :button-up-font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :button-down-font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :button-select-font "/home/john/quicklisp/local-projects/spill/fonts/LiberationMono-Regular.ttf"
        :colors (list :button-up-colors (list :base-color #xff888899
                                               :top-border #xffbbbbbb
                                               :bottom-border #xff444444
                                               :text-color #xff000000)
                       :button-down-colors (list :base-color #xff666677
                                                 :top-border #xff444444
                                                 :bottom-border #xffbbbbbb
                                                 :text-color #xff000000)
                       :button-select-up-colors (list :base-color #xff889999
                                                   :top-border #xffbbccbb
                                                   :bottom-border #xff445544
                                                   :text-color #xff000000)
                        :button-select-down-colors (list :base-color #xff667777
                                                         :top-border #xff445544
                                                         :bottom-border #xffbbccbb
                                                         :text-color #xff000000))
        :button-up-border-width 4
        :button-down-border-width 4
        :button-select-border-width 4))