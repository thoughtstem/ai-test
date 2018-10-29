#lang racket

(require (except-in "../lang.rkt" game mind mind<->game)
         "../games/cell-game.rkt"
         "../games/cell-game-renderer.rkt"
         posn)

(setup current-state
  
       (game my-game
             (posn 5 5))

       (mind a move-rand)

       (mind<->game a my-game
                    #:perception (build-channel #:from-game (thunk* #f) #;as-is)
                    #:intention  (build-channel #:from-mind mind->next-dir)))


(simulate current-state render)


