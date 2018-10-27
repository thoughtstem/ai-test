#lang racket

(require "./games/test-game.rkt")

(setup current-state

       (game my-game
             0
             #;(cell 'a 50 50 'green)
             #;(cell 'b 50 40 'red)
             #;(food 'f 20 20)
             )


       (mind dumb
             add1
             #;(when hungry? eat!))


       (mind<->game dumb my-game
                    #:perception (unlimited)
                    #:intention  (unlimited mind->game)
                    ))

(require 2htdp/universe)

(define (rt x)
  (sleep 0.5)
  (tick!   current-state)
  (render current-state))

(animate rt)

