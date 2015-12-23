#lang racket

(require "sprite.rkt")

(require racket/gui/base)
(require srfi/26)

(define postcard-size (sizen 500 (/ (* 500 148) 100)))

(define background-image (new background-image%
                              (path "./postcard.jpg")
                              (size postcard-size)))
;;
;; font defaults
;;
(define to-postno-font (make-font #:size 34
                                  #:smoothing 'smoothed
                                  #:size-in-pixels? #t))
(define to-address-font (make-font #:size 34
                                   #:face "HGGyoshotai"
                                   #:smoothing 'smoothed
                                   #:size-in-pixels? #t))
(define to-name-font (make-font #:size 60
                                #:face "HGGyoshotai"
                                #:smoothing 'smoothed
                                #:size-in-pixels? #t))

(define from-postno-font (make-font #:size 20
                                    #:smoothing 'smoothed
                                    #:size-in-pixels? #t))
(define from-address-font (make-font #:size 26
                                     #:face "HGGyoshotai"
                                     #:smoothing 'smoothed
                                     #:size-in-pixels? #t))
(define from-name-font (make-font #:size 40
                                  #:face "HGGyoshotai"
                                  #:smoothing 'smoothed
                                  #:size-in-pixels? #t))

;;
;; pos defaults
;;
(define to-name-pos (make-posn 230 110))
(define to-address-pos (make-posn 400 100))
(define to-postno1-pos (make-posn 210 50))
(define to-postno2-pos (make-posn 320 50))

(define from-name-pos (make-posn 40 320))
(define from-address-pos (make-posn 85 280))
(define from-postno1-pos (make-posn 25 605))
(define from-postno2-pos (make-posn 90 605))

;;
;; data renders
;;
(define address-render%
  (class sprite-list%
    (super-new)
    (init-field name postno1 postno2 address
                name-font name-pos
                postno1-font postno1-pos
                postno2-font postno2-pos
                address-font address-pos)
    (inherit append-sprite)
    (append-sprite (new vertical-text% (text name) (pos name-pos) (font name-font)))
    (append-sprite (new vertical-text% (text address) (pos address-pos) (font address-font)))
    (append-sprite (new horizontal-text% (text postno1) (pos postno1-pos) (font postno1-font)))
    (append-sprite (new horizontal-text% (text postno2) (pos postno2-pos) (font postno2-font)))))

(define to-address-render%
  (class address-render%
    (super-new
     (name-pos to-name-pos)
     (address-pos to-address-pos)
     (postno1-pos to-postno1-pos)
     (postno2-pos to-postno2-pos)
     (name-font to-name-font)
     (address-font to-address-font)
     (postno1-font to-postno-font)
     (postno2-font to-postno-font))))

(define from-address-render%
  (class address-render%
    (super-new
     (name-pos from-name-pos)
     (address-pos from-address-pos)
     (postno1-pos from-postno1-pos)
     (postno2-pos from-postno2-pos)
     (name-font from-name-font)
     (address-font from-address-font)
     (postno1-font from-postno-font)
     (postno2-font from-postno-font))))

;;
;;gui
;;
(define (fixed-size-mixin % #:width (width -1) #:height (height -1))
  ((cut <> %)
   (mixin (area<%>) ()
     (super-new)
     (when (>= width 0)
       (send* this
         (stretchable-width #f)
         (min-width width)))
     (when (>= height 0)
       (send* this
         (stretchable-height #f)
         (min-height height))))))

(define left-panel%
  (fixed-size-mixin
   #:width (sizen-width postcard-size)
   #:height (sizen-height postcard-size)
   (class vertical-panel% (super-new))))

(define right-panel%
  (class vertical-panel% (super-new)))

(define main-panel%
  (fixed-size-mixin
   #:height (sizen-height postcard-size)
   (class horizontal-panel% (super-new))))

(define main-frame%
  (class frame%
    (super-new (label "NEW YEAR CARD")
               (height (sizen-height postcard-size))
               (width 600))))

(define postcard-canvas%
  (fixed-size-mixin
   #:width 500 #:height 740
   (class canvas% (super-new)
     (inherit get-dc)
     (init-field sprites)
     (define/override (on-paint)
       (define dc (get-dc))
       (for ((s sprites))
         (send s draw dc))))))

;;
;; pdf writer
;;
(define (write-pdf path size sprites)
  (define dc (new pdf-dc% (output path)
                  ; 正しく印刷できないときはpdfに
                  ; はがきの画像を表示して
                  ; widthとheightを調整する
                  (width 400)
                  (height 592)
                  (interactive #f)))
  (send* dc
    (start-doc "")
    (start-page))
  (for ((s sprites))
    (send s draw dc))
  (send* dc
    (end-page)
    (end-doc)))

(define (main from-render to-render-list)
  (define to-renders to-render-list)
  (define (get-sprites)
    (list background-image from-render(car to-renders)))

  (define f (new main-frame%))
  (define main-panel (new main-panel% (parent f)))

  (define left-panel (new left-panel% (parent main-panel)))
  (define canvas (new postcard-canvas%
                      (parent left-panel)
                      (sprites (get-sprites))))

  (define right-panel (new right-panel% (parent main-panel)))
  
  (define next-button
    (new (fixed-size-mixin button% #:width 100 #:height 50)
         (parent right-panel)
         (label "NEXT")
         (callback (lambda (b e)
                     (set! to-renders (append (cdr to-renders) `(,(car to-renders))))
                     (set-field! sprites canvas (get-sprites))
                     (send canvas refresh-now)))))

  (define pdf-button
    (new (fixed-size-mixin button% #:width 100 #:height 50)
         (parent right-panel)
         (label "PDF")
         (callback
          (lambda (b e) 
            (match (put-file "Output PDF file" #f #f "out.pdf" "pdf" '() '(("PDF" "*.pdf")))
              (#f (void))
              (path (write-pdf path postcard-size
                               (cdr (get-sprites)))))))))
  
  (send f show #t))
  
;;
;; data -> render
;;
(define from-render
  (new from-address-render%
       (name "黒田 官兵衛")
       (postno1 "812") (postno2 "0044")
       (address "福岡県福岡市博多 九の六")))

(define to-renders
  (map (cut match <> ((list name pno1 pno2 addr)
                      (new to-address-render%
                           (name name)
                           (postno1 pno1)
                           (postno2 pno2)
                           (address addr))))
       '(("徳川 家康 様" "１００" "０００１" "東京都千代田区江戸城本丸一ノ二")
         ("石田 三成 様" "５２２" "０００７" "滋賀県彦根市古沢佐和山城三七")
         ("安国寺 恵瓊 様" "７３２" "００６８" "広島県広島市安芸安国寺金堂"))))

(main from-render to-renders)
