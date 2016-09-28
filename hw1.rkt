#lang class/0
(require 2htdp/image)
(require 2htdp/universe)

;;; Fundamentals of Computer Science 2
;;; Assignment 2 - Space Invaders
;;; Due: 16 January 2013

;; 2.1 Space Invaders

(define SCENE-DIM 500)

;; Ship Class
;; A Ship is a (new ship% Number)
(define-class ship%
  (fields x)
  ;; on-key : KeyEvent -> Ship
  ;; Moves the ship as specified by user input.
  (define (key-handler ke)
    (cond [(and (key=? "left" ke)
                (> (send this x)
                   (/ (image-width (send this draw)) 2)))
           (new ship% (sub1 (send this x)))]
          [(and (key=? "right" ke)
                (< (send this x)
                   (- (/ (image-width (send this draw)) 2)
                      SCENE-DIM)))
           (new ship% (add1 (send this x)))]
          [else this]))

  ;; draw : -> Image
  ;; Draws the Ship.
  (define (draw)
    (overlay (circle 8 "solid" "blue")
             (triangle 40 "solid" "green"))))

;; Bullet Class
;; A Bullet is a (new bullet% Number Number)
(define-class bullet%
  (fields x y)
  ;; step : -> Bullet
  ;; Produces the next bullet.
  (define (step)
    (new bullet% (send this x) (add1 (send this y))))
  ;; draw : -> Image
  ;; Draws the Bullet.
  (define (draw)
    (line 0 5 "black"))
  ;; off-screen? : -> Boolean
  ;; Is the Bullet off the screen?
  (define (off-screen?)
    (< (send this y) 0))
  ;; hit? : Invader -> Boolean
  ;; Has the bullet hit an invader?
  (define (hit? inv)
    (< (sqrt (+ (sqr (- (send this x) (send inv x)))
                (sqr (- (send this y) (send inv y))))) 10)))

;; Invader Class
;; An Invader is a (new invader% Number Number String)
(define-class invader%
  (fields x y direction)
  ;; step : -> Invader
  ;; Produces the next Invader.
  (define (step)
    (cond [(string=? (send this direction) "right")
           (new invader%
                (+ (send this x) (/ (image-width (send this draw)) 2))
                (send this y))]
          [else
           (new invader%
                (- (send this x) (/ (image-width (send this draw)) 2))
                (send this y))]))
  ;; draw : -> Image
  ;; Draws the Invader.
  (define (draw)
    (overlay/offset (circle 8 "solid" "red")
                    0 8
                    (ellipse 40 20 "solid" "red")))
  ;; wall-collide? : -> Boolean
  ;; Does the Invader collide with a wall?
  (define (wall-collide)
    (or (<= (- SCENE-DIM (send this x)) (/ (image-width (send this draw)) 2 ))
        (<= (send this x) (/ (image-width (send this draw)) 2 ))))
  ;; shot? : Bullet -> Boolean
  ;; Has the Invader been shot?
  (define (shot? bullet)
    (< (sqrt (+ (sqr (- (send this x) (send bullet x)))
                (sqr (- (send this y) (send bullet y))))) 10))
  ;; switch-dir : -> Invader
  (define (switch-dir)
    (if (string=? (send this direction) "right")
        (new invader% (send this x) (send this y) "left")
        (new invader% (send this x) (send this y) "right")))
  )

;; World Class
;; A World is a (new world% Ship [Listof Bullets] [Listof Invaders])
(define-class world%
  (fields ship bullets invaders)
  (define (on-tick)
    (if (ormap (λ (x) (wall-collide? x)) (send this invaders))
        (new world%
             (send this ship)
             (send this bullets)
             (map (λ (inv) (new invader%
                                (send inv x)
                                (+ (send inv y)
                                   (image-height (send inv draw)))
                                (send inv switch-dir)))))
        (new world%
             (send this ship)
             (map (λ (x) (send x step))
                  (filter (λ (y)
                            (ormap (λ (inv) (send y hit? inv))
                                   (send this invaders))) (send this bullets)))
             (map (λ (x) (send x step))
                  (filter (λ (y)
                            (ormap (λ (bul) (send y shot? bul))
                                   (send this bullets))) (send this invaders))))))
  (define (to-draw)
    (place-image (send (send this ship) draw)
                 (send (send this ship) x)
                 (- SCENE-DIM (/ (image-height (send (send this ship) draw)) 2))
                 (foldr (λ (c scn) (place-image (send c draw)
                                                (send c x)
                                                (send c y)
                                                scn))
                        (foldr (λ (c scn) (place-image (send c draw)
                                                (send c x)
                                                (send c y)
                                                scn))
                               (empty-scene SCENE-DIM SCENE-DIM)
                               (send this invaders)) (send this bullets))))
  (define (stop-when)
    (or (empty? (send this invaders))
        (ormap (λ (inv) (> (send inv y) 450)) (send this invaders))))
  (define (on-key ke)
    (cond[(or (string=? ke "left")
              (string=? ke "right"))
          (send (send this ship) key-handler)]
         [(string=? ke "space")
          (cons (new bullet%
                     (send (send this ship) x)
                     (- SCENE-DIM (/ (image-height
                                      (send (send this ship) draw)) 2)))
                (send this bullets))])))
