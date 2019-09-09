#lang racket

(require 2htdp/universe 2htdp/image)

(define WIDTH 500)
(define HEIGHT 500)
(define TEXT-SIZE 15)
(define SIZE 30)
(define INFO-COLOR "blue")
(define COLOR "red")

(define TEXT-X 20)
(define TEXT-UPPER-Y 20)
(define TEXT-LOWER-Y 20)

(struct interval (small big))

(define HELP-TEXT
  (text "up-arrow for larger numbers, down-arrow for smaller ones"
        TEXT-SIZE
        INFO-COLOR))

(define HELP-TEXT-TWO
  (text "Press = when your number is guessed; Press q to quit."
        TEXT-SIZE
        INFO-COLOR))

(define MT-SC
  (place-image/align
    HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
    (place-image/align
      HELP-TEXT-TWO TEXT-X TEXT-LOWER-Y "left" "bottom"
      (empty-scene WIDTH HEIGHT))))

(define (guess w)
  (quotient (+ (interval-big w)
               (interval-small w))
            2))

;; Returns an interval where the
;; upper bound is set to the max of
;; the lower value and a guess minus one.
(define (smaller w)
  (interval (interval-small w)
            (min (interval-small w) (sub1 (guess w)))))

;; Returns an interval where the
;; lower bound is set to the min of
;; the upper value and a guess plus one.
(define (bigger w)
  (interval (max (interval-big w) (add1 (guess w)))
            (interval-big w)))

;; Overlays the game UI on the empty scene.
(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

;; Overlays the end game UI on the empty scene.
(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) MT-SC))

;; Predicate for if the guess range has closed
;; in on a single number.
(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (handle-keys-for-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]))

(define (start lower upper)
  (big-bang (interval lower upper)
            (on-key handle-keys-for-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))
