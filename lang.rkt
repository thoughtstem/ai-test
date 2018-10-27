#lang racket

(provide setup
         tick
         tick!
         (struct-out state)
         (struct-out game)
         (struct-out mind)
         (struct-out mind<->game)
         (struct-out channel)
         unlimited)


(define-syntax-rule (setup id things ...)
  (define id (things->state things ...)))

(struct state         (games minds minds<->games) #:transparent)
(struct game          (id state tick) #:transparent)
(struct mind          (id state tick) #:transparent)
(struct mind<->game   (mind-id game-id perception intention) #:transparent)
(struct channel       (chain out tick) #:transparent)

(define (things->state . ts)
  (state (filter game? ts)
         (filter mind? ts)
         (filter mind<->game? ts)))

(define-syntax-rule (tick! s)
  (set! s (tick s)))

;This implements a simultaneous "Giant tick" function -- everything ticks together.
; Games, minds, and channels.
;(Other options exist.  For example, games and minds could take turns.
; As could various distinct minds.  Etc.  Lots of combinations...)
(define (tick s)
  (state
   (map (curry tick-game s) (state-games s))
   (map (curry tick-mind s) (state-minds s))
   (map (curry tick-mind<->game s) (state-minds<->games s))))

(define (tick-game s g)
  (struct-copy game g
               [state ((game-tick g)
                       (find-game-intentions s g)
                       (game-state g))]))

(define (tick-mind s m)
  (struct-copy mind m
               [state ((mind-tick m)
                       (find-mind-perceptions s m)
                       (mind-state m))]))

(define (tick-mind<->game s mg)
  (struct-copy mind<->game mg
               [perception (tick-channel (find-game mg s) (mind<->game-perception mg))]
               [intention  (tick-channel (find-mind mg s) (mind<->game-intention mg))]))

(define (tick-channel source p)
  ((channel-tick p) source p))


(define/contract (find-game mg s)
  (-> mind<->game? state? game?)
  (findf (λ(g)
           (eq? (game-id g)
                (mind<->game-game-id mg)))
         (state-games s)))

(define/contract (find-mind mg s)
  (-> mind<->game? state? mind?)
  (findf (λ(g)
           (eq? (mind-id g)
                (mind<->game-mind-id mg)))
         (state-minds s)))


(define (find-game-intentions s g)
  (define relevant-channels
    (filter
     (λ(mg)
       (eq? (game-id g)
            (mind<->game-game-id mg)))
     (state-minds<->games s)))

  (map (compose channel-out mind<->game-intention)
       relevant-channels))

(define (find-mind-perceptions s m)
  (define relevant-channels
    (filter
     (λ(mg)
       (eq? (mind-id m)
            (mind<->game-mind-id mg)))
     (state-minds<->games s)))

  (map (compose channel-out mind<->game-perception)
       relevant-channels))


(define (unlimited (f identity))
  (channel '() #f (λ(in me)
                    (struct-copy channel me
                                 [out (f in)]))))
