#| This file is part of Spill.

Spill is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
    
Spill is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Spill. If not, see <https://www.gnu.org/licenses/>. 

|# 

(defpackage :spill
  (:use #:cl)
  (:export
    #:init
    #:quit
    #:start-time
    #:end-time
    #:*time-keeping-strings*
    #:set-window-bg
    #:create-window
    #:create-typing-bar
    #:create-gui
    #:create-rect
    #:create-surface
    #:free-surface
    #:rect-collide-p
    #:create-button
    #:free-button
    #:create-image
    #:fill-surface
    #:open-font
    #:close-font
    #:draw-text
    #:add-callback
    #:blit
    #:get-keys
    #:get-relative-mouse-pos
    #:get-mouse-pos
    #:when-running
    #:get-page
    #:getm
    #:getm-page
    #:getm-page-test
    #:create-label
    #:add-page
    #:set-current-page
    #:open-window
    #:clear-window
    #:flip-window
    #:add-widget-callback
    #:get-event
    #:reset-events
    #:set-input-flags
    #:get-input-signals
    #:create-tab-frame
    #:add-tab
    #:get-tab
    #:tab-frame-get-page
    #:remove-widget-callback
    #:add-widget
    #:default
    #:click
    #:selected
    #:deselected
    #:fleeting-lmb-down
    #:all-pages
    #:click-and-drag
    #:hover-mouse
    #:*hover-mouse-time*
    #:create-info-box
    #:run-application
    #:process-page
    #:*delta-t*
    #:close-application))
