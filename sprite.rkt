#lang racket

(require racket/draw)
(provide (all-defined-out))

;;
;; geometry
;;
(define-struct sizen (width height) #:transparent)
(define-struct posn (x y) #:transparent)

;;
;; interfaces & traits & classes
;;
;; draw (dc<%> . -> . void)
(define sprite<%> (interface () draw))

(define sprite-list%
  (class* object% (sprite<%>)
    (super-new)
    (init-field (sprite-list '()))
    (define/public (append-sprite s)
       (set! sprite-list (append sprite-list `(,s))))
    (define/public (draw dc)
      (for ((s sprite-list))
        (send s draw dc)))))

(define background-image%
  (class* object% (sprite<%>)
    (super-new)
    (init-field path size)
    (field (image (read-bitmap path))
           (scale (/ (sizen-height size)
                     (send image get-height))))
    (define/public (draw dc)
      (send dc set-scale scale scale)
      (send dc draw-bitmap image 0 0)
      (send dc set-scale 1 1))))

(define textunit%
  (class* object% (sprite<%>) (super-new)
    (init-field text pos)
    (define/public (draw dc)
      (send dc draw-text text (posn-x pos) (posn-y pos)))))

(define first-textunit%
  (class textunit% (super-new)
    (init-field font)
    (define/override (draw dc)
      (send dc set-font font)
      (super draw dc))))

(define textunit-list%
  (class sprite-list% (super-new)
    (init-field text pos font)
    (field (splitted-text (map string (string->list text)))
           (first-text (car splitted-text))
           (rest-text (cdr splitted-text))
           (font-size (send font get-point-size))
           (font-size-half (/ font-size 2))
           (x0 (posn-x pos))
           (y0 (posn-y pos)))))

(define vertical-text%
  (class textunit-list%
    (super-new)
    (inherit append-sprite)
    (inherit-field font font-size font-size-half pos x0 y0 first-text rest-text)
    (define/private (next-y s y)
      (+ y (if (string=? s " ") font-size-half font-size)))
    (begin
      (append-sprite (new first-textunit% (text first-text) (pos pos) (font font)))
      (for/fold ((prev-y y0) (prev-text first-text))
                ((s rest-text))
        (let ((y (next-y s prev-y)))
          (append-sprite (new textunit% (text s) (pos (posn x0 y))))
          (values y s))))))

(define horizontal-text%
  (class textunit-list% (super-new)
    (inherit append-sprite)
    (inherit-field font font-size font-size-half pos x0 y0 first-text rest-text)
    ;; 半角スペースは文字サイズの半分で文字送り
    (define/private (next-x s x)
      (+ x (if (string=? s " ") font-size-half font-size)))
    ;;初期化 (textunit-list構築)
    (begin
      (append-sprite (new first-textunit% (text first-text) (pos pos) (font font)))
      (for/fold ((prev-x x0) (prev-text first-text))
          ((s rest-text))
        (let ((x (next-x s prev-x)))
          (append-sprite (new textunit% (text s) (pos (posn x y0))))
          (values x s))))))
