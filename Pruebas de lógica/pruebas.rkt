
#lang racket/gui

[define (replace_from_list ele x lista)
  (cond ((null? lista) '())
        ((equal? ele (car lista)) (append (list x) (replace_from_list ele x (cdr lista))))
        (else (append (list (car lista)) (replace_from_list ele x (cdr lista))))
  )
 ]
[define (delete_last_element lista)
  (reverse (cdr (reverse lista)))
  ]
[define (get_last_element lista)
  (car (reverse lista))
  ]

[define (get_full_deck)
 (shuffle '(11 12 13 14 21 22 23 24
        31 32 33 34 41 42 43 44
        51 52 53 54 61 62 63 64
        71 72 73 74 81 82 83 84
        91 92 93 94 101 102 103 104
        111 112 113 114 121 122 123 124
        131 132 133 134))]
[define (bCEj players)
  (reverse (bCEj_aux players (get_full_deck)))
   
  ]
[define (bCEj_aux players deck)
  (cond((null? players) (append (crupier deck)))
       (else (cons (car (draw_cards 2 deck)) (bCEj_aux (cdr players) (cadr (draw_cards 2 deck))))))
  ]

;;Devuelve el juego del crupier y el deck
[define (crupier deck)
  (crupier_aux (car (draw_cards 2 deck)) (cadr (draw_cards 2 deck)))
  ]
[define (crupier_aux hand deck)
  (cond
       ((<= (add_cards hand) 16) (list (append (car (draw_cards 1 deck)) hand) (get_last_element (draw_cards 1 deck) ) ))
       (else (list hand deck))
  )
      ]
  
  

 ;;retorna (cartas, deck sin las carta)
[define (draw_cards num deck)
  (list (delete_last_element (draw_cards_aux num deck)) (get_last_element (draw_cards_aux num deck)))]
[define (draw_cards_aux num deck)
  (cond ((equal? num 0) (list deck)) ;;(list deck))
        (else (append (list (car deck)) (draw_cards_aux (- num 1) (cdr deck)))
              ))]

;;( (1 (5 4)) (2 (1 10)) (3 (10 11)) )  (jugador (carta1 carta2...))

;; si la carta es un as
[define (as? card)
  (cond ((and (>= card 11)
             (<= card 14) (integer? card)) #t)
        (else #f))]

;;numero que representa la carta
[define (get_card_value card)
  (cond ((<= 111 card) 10)
        ((as? card) 11)
        (else (get_card_value_aux card)))]
  
[define (get_card_value_aux card)
  (round (/ card 10))
  ]


;; para saber si la mano es un blackjack

[define (ten? card)
  (cond ((equal? (get_card_value card) 10) #t)
        (else #f))]
[define (blackjack? cards)
  (cond((and (equal? (length cards) 2)
             (or (and (ten? (car cards)) (as? (cadr cards)))
                 (and (as? (car cards)) (ten? (cadr cards))))) #t)
       (else #f))]
[define (has_as? hand)
  (cond ((null? hand) #f)
        ((as? (car hand)) #t)
        (else (has_as? (cdr hand))))]
;;sumar cartas
[define (add_cards set)
  (add_cards_aux set)]
  

[define (add_cards_aux set)  
  (cond((null? set) 0)
      (else (+ (get_card_value (car set)) (add_cards_aux (cdr set)))))]

[define (keep_playing? hand)
  (cond ((> (add_cards hand) 21) #f)
        (else #t))]
;;[define (

[define (winners game)
  (winners_aux (car game) (cdr game))  
  ]
[define (winners_aux crupie players)
  (cond ((null? players) '())
        ((blackjack? (car players)) (append (list "Won") (winners_aux crupie (cdr players))))
        ((not (keep_playing? crupie)) (append (list "Won") (winners_aux crupie (cdr players))))
        ((not (keep_playing? (car players))) (append (list "Lost") (winners_aux crupie (cdr players))))
        ((< (add_cards (car players)) (add_cards crupie)) (append (list "Lost") (winners_aux crupie (cdr players))))
        (else (append (list "Won") (winners_aux crupie (cdr players)))))]
         
                     
  
  