#lang racket

(require (except-in "./lang.rkt" game mind mind<->game)
         "./games/test-game.rkt"
         "./games/test-game-renderer.rkt")

(setup current-state

       (game my-game
             0
             #;(cell 'a 50 50 'green)
             #;(cell 'b 50 40 'red)
             #;(food 'f 20 20)
             )


       (mind adder
             add1
             #;(when n (add1 n))
             #;(when hungry? eat!))


       (mind<->game adder my-game
                    #:perception (build-channel #:from-game as-is)
                    #:intention  (build-channel #:from-mind mind->next-number)))

(require (only-in 2htdp/universe animate))

(define (rt x)
  (sleep 0.5)
  (tick!   current-state)
  (render  current-state))

(animate rt)

