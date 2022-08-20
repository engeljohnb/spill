(defpackage :spill
  (:use #:cl)
  (:export
    #:init
    #:quit
    #:--print
    #:set-window-bg
    #:create-window
    #:create-typing-bar
    #:create-gui
    #:create-rect
    #:create-surface
    #:create-button
    #:create-image
    #:fill-surface
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
