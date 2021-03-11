#lang racket/gui


(require "logic.rkt")


(define imgFondo (make-object bitmap% "bg.jpg"))
(define image2 (make-object bitmap% "pika.jpg"))


(define frame (new frame%
                   [label "BlackCEJack"]
                   [width 800]
                   [height 600]
                   [style '(no-resize-border)]))


(define msg (new message% [parent frame]
                          [label "No events so far..."]))


(define canvas (new canvas% [parent frame]
             [min-width 800]	 
   	     [min-height 550]
             [paint-callback
              (lambda (canvas dc)
                (send dc draw-bitmap imgFondo 0 0)
                (send dc draw-bitmap image2 100 50)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))]))


(define panel (new horizontal-panel%
                   [parent frame]
                   [alignment '(center bottom)]
                   [vert-margin 0]))
(new button% [parent panel]
             [label "Stay"]
             [callback (lambda (button event)
                         (send (send canvas get-dc) clear)
                         (send (send canvas get-dc) draw-text "Don't Panic!" 0 0))])
(new button% [parent panel]
             [label "Hit"])


(define jugadores (new dialog% (label "Jugadores")))
(define p1(new text-field% [parent jugadores] [label "Jugador 1"]))
(define p2(new text-field% [parent jugadores] [label "Jugador 2"]))
(define p3(new text-field% [parent jugadores] [label "Jugador 3"]))
(new button% [parent jugadores]
             [label "Ok"]
             [callback (lambda (button event)
                         (send jugadores show #f)
                         (iniciarPartida (send p1 get-value) (send p2 get-value) (send p3 get-value)))])


(define resultados (new dialog% (label "Resultados")))
(new message% [parent resultados]
                          [label (string-append "Jugador 1 " "result2")])
(new message% [parent resultados]
                          [label (string-append "Jugador 2 " "result3")])
(new message% [parent resultados]
                          [label (string-append "Jugador 3 " "result4")]) 

(define (iniciarPartida j1 j2 j3)
  (display j1)
  (display "\n")
  (display j2)
  (display "\n")
  (display j3))

(send frame show #t)
(send resultados show #t)
(send jugadores show #t)




