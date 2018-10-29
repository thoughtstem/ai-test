#lang racket

(provide
 (rename-out [my-make-game game])
 (rename-out [my-make-mind mind])
 (rename-out [make-mind<->game mind<->game])

 mind->next-dir
 move-rand)

(require "../lang.rkt")

;Hook for translating user's syntax into internal
;  game representations
(define-syntax-rule (my-make-game id t)
  (make-game id (game->game) t))

;Hook for translating user's syntax into internal
;  mind representations
(define-syntax-rule (my-make-mind id t)
  (make-mind id (mind->mind) (->func t)))

;Simple game rules.  The first intention BECOMES
;  the new game state...
(define (game->game)
  (on-tick #:normal        no-change
           #:on-messages   move-cells))


(define (mind->mind)
  (on-tick #:normal        update-mind
           #:on-messages   update-mind))


;Define mind
(define (update-mind ms (perceptions '()))
  (func-call ms #f))

(define (mind->next-dir m)
  (func-out m))

(define move-rand
  (thunk* (rand-dir)))

(define (rand-dir)
  (define l '(up left right down))

  (list-ref l (random 4)))

;Define game

(require posn)

(define (move-cells ms perceptions)
  (define p (first perceptions))
  (match p
    ['up  (posn-add ms (posn 0 -1))]
    ['down  (posn-add ms (posn 0 1))]
    ['left  (posn-add ms (posn -1 0))]
    ['right  (posn-add ms (posn 1 0))]
    [else ms]))
