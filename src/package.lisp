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
    #:create-surface
    #:create-button
    #:create-image
    #:fill-surface
    #:add-callback
    #:blit
    #:get-keys
    #:open-window
    #:add-widget-callback
    #:get-event
    #:reset-events
    #:set-input-flags
    #:get-input-signals
    #:remove-widget-callback
    #:add-widget
    #:run-application
    #:close-application))
