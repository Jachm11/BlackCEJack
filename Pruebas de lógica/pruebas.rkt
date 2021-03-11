
#lang racket/gui

[define (delete_from_list ele lista)
  (cond ((null? lista) '())
        ((equal? ele (car lista)) (delete_from_list ele (cdr lista)))
        (else (append (list (car lista)) (delete_from_list ele (cdr lista))))
  )
 ]

[define (get_full_deck)
 (shuffle '(11 12 13 14 21 22 23 24
        31 32 33 34 41 42 43 44
        51 52 53 54 61 62 63 64
        71 72 73 74 81 82 83 84
        91 92 93 94 101 102 103 104
        111 112 113 114 121 122 123 124
        131 132 133 134))]

;;(carta, deck sin a carta)
[define (draw_card deck)
  (list (car deck) (cdr deck))]

;;( (1 (5 4)) (2 (1 10)) (3 (10 11)) )  (jugador (carta1 carta2...))

;;numero que representa la carta

[define (get_card_value card)
  (round (/ card 10))
  ]
;; para saber si la mano es un blackjack
[define (blackjack? cards)
  (cond((and (equal? (length cards) 2)
             (or (and (equal? (get_card_value (car cards)) 1) (equal? (get_card_value (cadr cards)) 10))
                 (and (equal? (get_card_value (car cards)) 10) (equal? (get_card_value (cadr cards)) 1)))) #t)
       (else #f))]

;;sumar cartas
[define (add_cards set)
  (add_cards_aux (cadr set))]
  

[define (add_cards_aux set)  
  (cond((null? set) 0)
      (else (+ (car set) (add_cards_aux (cdr set)))))]
  
;;[define (  
;;[define (winner? crupier player) ]
  