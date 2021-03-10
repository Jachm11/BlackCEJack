#lang racket/gui


(require "logic.rkt")

;(provide print-cake)
 
; draws a cake with n candles
(define (print-cak n)
  (send dialog show #t))
 

(define image1 (make-object bitmap% "bj.jpg"))

(define frame (new frame%
                   [label "Example"]
                   [width 800]
                   [height 600]
                   [style '(no-resize-border)]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc draw-bitmap image1 0 0)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])

(new button% [parent frame]
             [label "Right"]
             [callback (lambda (button event)
                         (print-cake (random 30)))])

; Create a dialog
(define dialog (instantiate dialog% ("Example")))
 
; Add a text field to the dialog
(new text-field% [parent dialog] [label "Your name"])
 
; Add a horizontal panel to the dialog, with centering for buttons
(define panel (new horizontal-panel% [parent dialog]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(new button% [parent panel] [label "Cancel"])
(new button% [parent panel] [label "Ok"])
(when (system-position-ok-before-cancel?)
  (send panel change-children reverse))


; Show the frame by calling its show method
(send frame show #t)

;(print-cake (random 30))


