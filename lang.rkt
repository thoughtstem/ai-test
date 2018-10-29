#lang racket

(provide setup
         tick
         tick!
         (struct-out state)
         (struct-out game)
         (struct-out mind)
         (struct-out mind<->game)
         (struct-out channel)

         (struct-out func)
         ->func func-call chain
         
         build-channel
         as-is

         no-change
         directly-set-from

         make-game
         make-mind
         make-mind<->game

         simulate

         on-tick) 


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

(define (on-tick #:normal normal
                 #:on-messages on-messages)
  (λ(messages current-state)
    (if (or (empty? messages)
            (not (first messages)))
        (normal current-state)
        (on-messages current-state messages))))

(define-syntax-rule (tick-x all part part-kind part-tick part-state find-in-channels)
  (begin
    (define is (find-in-channels all part))

    (struct-copy part-kind part
                 [state ((part-tick part)
                         is
                         (part-state part))])))

(define (tick-game s g)
  (tick-x s g game game-tick game-state find-game-intentions))

(define (tick-mind s m)
  (tick-x s m mind mind-tick mind-state find-mind-perceptions))

(define (tick-mind<->game s mg)
  (struct-copy mind<->game mg
               [perception (tick-channel (find-game mg s) (mind<->game-perception mg))]
               [intention  (tick-channel (find-mind mg s) (mind<->game-intention  mg))]))

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


(define (build-channel #:from-mind (fm #f) #:from-game (fg #f))
  (channel '() #f (λ(in me)
                    (struct-copy channel me
                                 [out ((or fm fg) (as-is in))]))))


(define (as-is g-or-m)
  (cond
    [(game? g-or-m) (game-state g-or-m)]
    [(mind? g-or-m) (mind-state g-or-m)]
    [else g-or-m]))


(struct func (do out))

(define (->func f)
  (func f #f))

(define (func-call fc x)
  (define f (func-do fc))
  (func f (f x)))


(define (chain . fcs)
  (->func
   (apply compose (map func-do fcs))))


;Construction functions.  Syntactic sugars and stuff.
(define-syntax-rule (make-game id game->game s)
  (game 'id s game->game))

(define-syntax-rule (make-mind id mind->mind s)
  (mind 'id s mind->mind))

(define-syntax-rule (make-mind<->game game-id mind-id
                                      #:perception perception
                                      #:intention  intention)
  (mind<->game 'game-id 'mind-id
               perception intention))


(define no-change (λ(s (ms '())) s))

(define (directly-set-from f)
  (λ(gs intentions)
    (first intentions)))




(require (only-in 2htdp/universe animate))

(define (simulate current-state render)
  (define (rt x)
    (sleep 0.5)
    (tick!   current-state)
    (render  current-state))

  (animate rt))
