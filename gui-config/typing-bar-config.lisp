#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

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
                                                      