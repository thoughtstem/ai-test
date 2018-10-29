#lang racket

(require (except-in "../lang.rkt" game mind mind<->game)
         "../games/test-game.rkt"
         "../games/test-game-renderer.rkt")

(setup current-state
  
       (game my-game 0)

       (mind adder   add1)

       (mind<->game adder my-game
                    #:perception (build-channel #:from-game as-is)
                    #:intention  (build-channel #:from-mind mind->next-number)))


(simulate current-state render)


