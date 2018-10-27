#lang racket

;Can we reduce this boiler plate?
(provide setup
         tick
         
         
         (rename-out [make-game game])
         (rename-out [make-mind mind])
         (rename-out [make-mind<->game mind<->game])
         render
         
         cell
         food
         limit-to
         unlimited)

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
  ts)





;Actual game implementation

(struct cell-game (cells food) #:transparent)
(struct cell (id x y color) #:transparent)
(struct food (id x y color) #:transparent)

(define (game->game intentions g)
  g)




;Actual mind implementation

(struct cell-mind ())

(define (mind->mind perceptions m)
  m)



;Perception and intention implementations

(define unlimited
  (channel '() #f (thunk* #f) ;Function from (chain, input) -> out (stored in second field)
           ))

(define (limit-to eid)
  (channel '() #f (thunk* #f)))




;Rendering

(define (render s)
  (circle 40 'solid 'red))







