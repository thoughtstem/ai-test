#lang racket

(provide
 (rename-out [my-make-game game])
 (rename-out [my-make-mind mind])
 (rename-out [make-mind<->game mind<->game])

 mind->next-number)

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
           #:on-messages   (directly-set-from first)))


(define (mind->mind)
  (on-tick #:normal        no-change
           #:on-messages   update-mind))

(define (update-mind ms perceptions)
  (func-call ms
             (first perceptions)))

(define (mind->next-number m)
  (func-out m))

