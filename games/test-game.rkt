#lang racket

;This was supposed to be super simple, but it's a mess.
;  Should just pass well-defined messages, not entire game states...

;SIMPLIFY before implemention more complex games.


(provide setup
         tick
         tick!
         unlimited
  
         (rename-out [make-game game])
         (rename-out [make-mind mind])
         (rename-out [make-mind<->game mind<->game])
         render

         mind->game)

(require 2htdp/image
         "../lang.rkt")


;Construction functions.  Syntactic sugars and stuff.
(define-syntax-rule (make-game id . things)
  (game 'id (things->game-state 'things) game->game))

(define-syntax-rule (make-mind id alg)
  (mind 'id (things->mind-state alg) mind->mind))

(define-syntax-rule (make-mind<->game game-id mind-id
                                      #:perception perception
                                      #:intention  intention)
  (mind<->game 'game-id 'mind-id
               perception intention))


(define (things->game-state ts)
  (first ts))

(define (things->mind-state ts)
  (list #f ts))

;Rendering

(define (game->game intentions gs)
  (if (or (empty? intentions)
          (not (first intentions)))
      gs
      (game-state (first intentions))))

(define (mind->mind perceptions ms)
  (if (or (empty? perceptions)
          (not (first perceptions)))
      ms
      (list
       (first perceptions)
       (second ms))))

(define (mind->game m)
  (define prev-m (first (mind-state m)))
  (define my-f   (second (mind-state m)))
  
  (if prev-m
      (struct-copy game prev-m
               [state (my-f (game-state prev-m))])
      #f))

(define (render s)
  (match-define 
    (state gs ms mgs)
    s)

  (define g (first gs))
  (define m (first ms))
  
  (match-define
    (mind<->game gi mi p i)
    (first mgs))
  
  (beside
   (render-game g)
   (above (render-channel p)
          (render-channel i))
   (render-mind m)))


(define (render-game g)
  (overlay
   (text (~a (game-id g) " : " (game-state g)) 18 'white)
   (square 200 'solid 'black)))

(define (render-mind m)
  (define last-seen (first (mind-state m)))
  (overlay
   (text (if last-seen
             (~a (mind-id m) " : " (game-state last-seen))
             "#f") 18 'white)
   (square 200 'solid 'black)))

(define (render-channel p)
  (define o (channel-out p))
  (overlay
   (scale 0.5
          (cond
            [(game? o) (render-game o)]
            [(mind? o) (render-mind o)]
            [else (text (~a o) 24 'black)]))
   (square 200 'solid 'gray)))