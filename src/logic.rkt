#lang racket/gui
;       ____________________________________________
;______/Funciones de lógica que necesita la interfaz

(provide draw_card)
(provide keep_playing?)
(provide winners)
(provide bCEj)
(provide first)
(provide rest)
(provide second)
(provide third)

;       _______________________________________
;______/Funciones de procesamiento de listas
(define (first list)
  (car list))
(define (rest list)
  (cdr list))
(define (second list)
  (cadr list))
(define (third list)
  (caddr list))

;Función que elimina el último elemento de una lista
;Input: Una lista 
;Output: La lista si el último elemento
[define (delete_last_element lista)
  (reverse (cdr (reverse lista)))
  ]

;Función que retorna el último elemento de una lista
;Input: Una lista 
;Output: El último elemento de la lista
[define (get_last_element lista)
  (car (reverse lista))
  ]

;Función que reemplaza el elemento en la posición dada con el elemento dado
;Input: Un número entero (índice de la lista), un elemento y una lista
;Output: La lista con el elemento dado en la posición especificada
[define (replace index ele lista)
  (cond((<= (length lista) index) lista)
       ((equal? index 0) (append (list ele) (cdr lista)))
        (else (append (list (car lista)) (replace (- index 1) ele (cdr lista)))))]



;       __________________________________________________________
;______/Funciones principales que utiliza la interfaz directamente

;Función para inicializar el juego de Blackjack
;Input: Una lista de strings con los nombres de los jugadores
;Output: Una lista con: una lista de las cartas, una lista con el juego del crupier y las listas con las 2 cartas iniciales de los jugadores
[define (bCEj players)
  (reverse (bCEj_aux players (get_full_deck)))
  ]
[define (bCEj_aux players deck)
  (cond((null? players) (append (crupier deck)))
       (else (cons (car (draw_cards 2 deck)) (bCEj_aux (cdr players) (cadr (draw_cards 2 deck))))))
  ]

;Función que indica si el jugador puede seguir pidiendo cartas
;Input: Una lista con las cartas del jugador
;Output: #t si la suma de las cartas es menor a 21 y #f si es mayor o igual a 21
[define (keep_playing? hand)
  (cond ((>= (add_cards hand) 21) #f)
        (else #t))
  ]

;Función para que el jugador pida una carta
;Input: El número de jugador que está solicitando una carta y una lista con el juego completo: ((mazo) (cartas del crupier) (cartas J1)...)
;Output: Retorna la lista con el juego completo pero le agrega una carta más al jugador especificado y remueve esa carta de la lista del mazo
[define (draw_card player game)
  (replace 0 (second (draw_cards 1 (first game))) (replace (+ player 1) (append (list-ref game (+ player 1)) (first (draw_cards 1 (first game)))) game))
  ]

;Función que determina la condición final de cada jugador (Ganó, Perdió, Empató, Blackjack, Blackjack del cupier)
;Input: Una lista on el juego completo: ((mazo) (cartas del crupier) (cartas J1)...)
;Output: Una lista de strings con la condición de cada jugador ("Won" "Tie" "Lost")
[define (winners game)
  (winners_aux (car game) (cdr game))  
  ]
[define (winners_aux crupie players)
  (cond ((null? players) '())
        ((blackjack? (car players)) (append (list "BlackJack") (winners_aux crupie (cdr players))))
        ((blackjack? crupie) (append (list "Crupier BlackJack") (winners_aux crupie (cdr players))))
        ((not (keep_playing? (car players))) (append (list "Lost") (winners_aux crupie (cdr players))))
        ((not (keep_playing? crupie)) (append (list "Won") (winners_aux crupie (cdr players))))
        ((< (add_cards (car players)) (add_cards crupie)) (append (list "Lost") (winners_aux crupie (cdr players))))
        ((= (add_cards (car players)) (add_cards crupie)) (append (list "Tie") (winners_aux crupie (cdr players))))
        (else (append (list "Won") (winners_aux crupie (cdr players)))))
  ]


;       __________________________________________________________________
;______/Funciones secundarias que se utilizan en las funciones principales


;Función que retorna el mazo completo de cartas en orden aletorio
;Input: No recibe entradas
;Output: Una lista con 52 números enteros que representan todas las cartas del mazo
[define (get_full_deck)
 (shuffle '(11 12 13 14 21 22 23 24
        31 32 33 34 41 42 43 44
        51 52 53 54 61 62 63 64
        71 72 73 74 81 82 83 84
        91 92 93 94 101 102 103 104
        111 112 113 114 121 122 123 124
        131 132 133 134))
  ]

;Función que retorna las cartas del juego completo del crupier
;Input: El deck con el que juega el crupier
;Output:Una lista con: una lista con las cartas del crupier y el mazo sin las cartas del crupier
[define (crupier deck)
  (crupier_aux (car (draw_cards 2 deck)) (cadr (draw_cards 2 deck)))
  ]
[define (crupier_aux hand deck)
  (cond
       ((<= (add_cards hand) 16) (list (append (car (draw_cards 1 deck)) hand) (get_last_element (draw_cards 1 deck) ) ))
       (else (list hand deck))
  )
      ]
  
;Función para sacar cierta cantidad de cartas de un deck
;Input: Un número entero (cantidad de cartas) y una lista con el deck
;Output: Una lista que contiene una lista de las cartas que se sacaron y una lista del mazo sin las cartas
[define (draw_cards num deck)
  (list (delete_last_element (draw_cards_aux num deck)) (get_last_element (draw_cards_aux num deck)))]
[define (draw_cards_aux num deck)
  (cond ((equal? num 0) (list deck)) 
        (else (append (list (car deck)) (draw_cards_aux (- num 1) (cdr deck)))
              ))]

        
;Función que reemplaza el valor de un As de 11 a 1
;Input: Una lista de cartas
;Output: La misma lista con un As que vale 1
[define (replace_as_value hand)
  (cond ((null? hand) '())
        ((as? (car hand)) (append (list 11.1) (cdr hand)))
        (else (append (list (car hand)) (replace_as_value (cdr hand)))))
  
 ]

;Función para determinar el valor númerico de una carta
;Input:El número que representa la carta
;Output: El valor numérico de dicha carta
[define (get_card_value card)
  (cond ((<= 111 card) 10)
        ((as? card) 11)
        (else (get_card_value_aux card)))]
[define (get_card_value_aux card)
  (round (/ card 10))
  ]


;Función que determina si una carta es un As
;Input: El número que representa la carta
;Output: #t si representa un As con valor de 11 y #f si es otra carta
[define (as? card)
  (cond ((and (>= card 11)
             (<= card 14) (integer? card)) #t)
        (else #f))]

;Función que determina si una carta representa un valor numérico de 10
;Input: El número que representa la carta
;Output: #t si es una carta representa un valor numérico de 10 y #f si es otra carta
[define (ten? card)
  (cond ((equal? (get_card_value card) 10) #t)
        (else #f))
  ]

;Función que determina si las cartas son un Blackjack
;Input: Una lista con las cartas
;Output: #t si se formó un Blackjack y #f en el caso contrario
[define (blackjack? cards)
  (cond((and (equal? (length cards) 2)
             (or (and (ten? (car cards)) (as? (cadr cards)))
                 (and (as? (car cards)) (ten? (cadr cards))))) #t)
       (else #f))
  ]

;Función que determina si en las cartas hay un As
;Input: Una lista con las cartas
;Output: #t si tiene al menos un As y #f en el caso contrario
[define (has_as? hand)
  (cond ((null? hand) #f)
        ((as? (car hand)) #t)
        (else (has_as? (cdr hand))))]

;Función que suma las cartas de una mano
;Input: Una lista con las cartas
;Output: La sumatoria de dichas cartas, en el caso de que contenga un As y se pase de 21, cambia automáticamente su valor
[define (add_cards hand)
  (cond ((and (has_as? hand)(> (add_cards_aux hand) 21)) (add_cards (replace_as_value hand)))
        (else (add_cards_aux hand)))]
[define (add_cards_aux hand)  
  (cond((null? hand) 0)
      (else (+ (get_card_value (car hand)) (add_cards_aux (cdr hand)))))
  ]








;;crupier tiene blackjack
;(winners '((121 12) (12 65) (12 13) (121 64 131)))

;; Jugador 1 con blackjack
;(winners '((121 12) (121 12) (12 13) (121 64 131)))

;;j1 empata con 12, j2 gana con 13, j3 pierde con 10
;(winners '((12 12) (12 12) (34 113) (64 43)))
                     
  
  