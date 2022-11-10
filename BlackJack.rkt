#lang racket
; Kelly Ngoc Hoang

; BlackJack
;
; The best hand is 21
; A hand has at least 2 cards
; A total value is the sum of cards in the hand
;
; Whoever is closer to 21 wins
; Whoever gets BlackJack at the first 2 cards automatically win
; BlackJack is a combination of an Ace and a 10-value card

(define faces '(2 3 4 5 6 7 8 9 10 J Q K A))
(define suits '("♦" "♥" "♠" "♣"))

;;;;;;;;;;;;;;;;; Create a card
;;;;;;;;;;;;;;;;; A card has a value of face and suit
(define (make-card face suit)
 (cons face suit))


;;;;;;;;;;;;;;;;; Create a shuffled deck of cards
(define make-deck
  (shuffle (for*/list ([face faces] [suit suits]) 
    (make-card face suit))))

(define thedeck make-deck)


;;;;;;;;;;;;;;;;; Evaluate the best possible values of player's cards
;;;;;;;;;;;;;;;;; Ace is evaluated as either 1 or 11
(define (eval-hand hand [aces-count 0])
  (letrec ([ace-val (lambda (values aces-count) 
                        (if (> values 21)
                            (if (> aces-count 0)
                                (ace-val (- values 10) (- aces-count 1))
                                values)
                            values))]
           
           [total (lambda (lst) 
                    (if (empty? lst) 0
                        (+ (first lst) (total (rest lst)))))]

           [convert-value (lambda (face) 
                            (cond
                              [(equal? 'J face) 10]
                              [(equal? 'Q face) 10]
                              [(equal? 'K face) 10]
                              [(equal? 'A face)
                               (set! aces-count (+ 1 aces-count)) 11] 
                              [else face]))]
           
           [map-values (map convert-value (map car hand))] 

           )
    ; returns the total value of cards in the hand
    (ace-val (total map-values) aces-count))) 


;;;;;;;;;;;;;;;;; Deals two cards from the deck to a hand
;;;;;;;;;;;;;;;;; Removing drawn cards from the deck (use "remq*")
(define (deal! deck)
  (letrec ([thehand (cons (first deck) (list (first (rest deck))))]) 
    (set! thedeck (remq* thehand deck)) 
    thehand))


;;;;;;;;;;;;;;;;; Add a new card to the hand
;;;;;;;;;;;;;;;;; Remove the hit card from the deck
(define (hit! deck hand) 
    (letrec ([thehand (cons (first deck) hand)])
      (set! thedeck (remq* thehand deck)) 
      thehand)) 




;;;;;;;;;;;;;;;;; Display the full or partial hand depending on parameters
;;;;;;;;;;;;;;;;; Parameters: hand - the hand to display
;                             how - display either 'Full or 'Part of the hand
;                             description - string to be displayed before the actual hand
(define (show-hand hand how description)
  (letrec ([showhand 
            (cond
              [(equal? 'Full how) hand]
              [(equal? 'Part how)
               (if (empty? hand)
                   (list "*****")
                   (append (list "*****") (rest hand)))])])
    (display description)
    (displayln showhand)))

;;;;;;;;;;;;;;;;; Defines phayer and dealer hand
(define playerhand (deal! thedeck))
(define dealerhand (deal! thedeck))

;;;;;;;;;;;;;;;;; Start BlackJack
(displayln "♦♥♠♣ Welcome to BlackJack ♦♥♠♣")
(displayln "♦ Whoever gets BlackJack (21) first will win")
(displayln "♥ Let's try your luck and have fun!")
(displayln "♠ Type '(start)' to start a new game.")

(define (start)
  (show-hand dealerhand 'Part "The dealer has: ")
  (show-hand playerhand 'Full "You have: ")
  (cond
    ((> (eval-hand playerhand) 21)
     (begin (displayln "Busted!")
            ))

    ((= (eval-hand playerhand) 21)
     (begin (displayln "BlackJack!")
            ))
    (else (begin
            (displayln "Hit (h) or Miss (m)?")
            ; Reading user input
            (letrec ([user-input (read)]
                     [game (lambda ()
                             (cond
                               ; 1: hit -- add a new card
                               [(equal? 'h user-input) 
                                (set! playerhand (hit! thedeck playerhand))
                                (if (< (eval-hand dealerhand) 17)
                                    (set! dealerhand (hit! thedeck dealerhand)) 0)
                                (start)]
                               
                               ; 2: miss -- stop the game
                               [(equal? 'm user-input) ; passes the turn and compares playerhand to dealerhand
                                (if (> (eval-hand playerhand) (eval-hand dealerhand))
                                    (displayln "You win!")
                                    
                                    (if (< (eval-hand playerhand) (eval-hand dealerhand))
                                        (displayln "You lose!")
                                         
                                        (if (equal? (eval-hand playerhand) (eval-hand dealerhand))
                                            (displayln "You tie!") "")))]))])
                   
              (game))
  ))))