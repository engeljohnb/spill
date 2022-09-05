#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

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
