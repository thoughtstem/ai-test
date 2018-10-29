#lang racket

(provide render)

(require "../lang.rkt"
         2htdp/image)

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
  (define last-seen (func-out (mind-state m)))
  (overlay
   (text (if last-seen
             (~a (mind-id m) " : " last-seen)
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