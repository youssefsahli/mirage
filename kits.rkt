#lang racket

(provide (struct-out sample)
         make-kit-pict
         make-sample-pict
         make-kit
         make-kit-btmp
         kit?)

(require gregor
         racket/draw
         pict
         pict/shadow)

(define opaque-sample-image (bitmap "opaque.png"))
(define non-opaque-sample-image (bitmap "non-opaque.png"))
(define unknown-sample-image (bitmap "unknown.png"))

(struct/contract sample
                 ([id number?]
                  [opaque? boolean?]
                  [valid? boolean?])
                 #:transparent)

(define (blank-sample? sample)
  (sample-valid? sample))

(define (make-sample-name sample prefix)
  (~a prefix 
      (if (sample-opaque? sample)
          'O
          'T)
      (sample-id sample)))
                         

(define (kit? kit)
  (listof sample?))

(define/contract (make-kit size max-positives opaque?)
  (-> positive? natural? boolean? (listof sample?))
  (let ([K (build-list size
                       (λ (id)
                         (sample
                          id
                          opaque?
                          (>= id max-positives))))])
    (shuffle K)))

(define/contract (make-sample-pict sample [size 50] [hidden? #f])
  (->* (sample?) (positive? boolean?) pict?)
  (ct-superimpose
   (colorize (filled-rectangle size size)
             (if hidden?
                 "light slate gray"
                 (if (sample-valid? sample)
                     "white"
                     "coral")))
   (vc-append
    (scale-to-fit
     (bitmap
      (if hidden?
          unknown-sample-image
          (if (sample-opaque? sample)
              opaque-sample-image
              non-opaque-sample-image))) size size)
    (text (~a (make-sample-name sample 'A))))))

(define/contract (make-kit-pict kit [new-line-at 10] [last-line (blank)] [hidden? #f])
  (->* (kit?) (positive? pict? boolean?) pict?)
  (cond
    [(empty? kit) last-line]
    [else
     (let-values ([(splited-samples sample-rest) (split-at kit new-line-at)])
       (let ([samples-pict (apply
                            hc-append
                            (map
                             (λ (s) (make-sample-pict s 50 hidden?))
                             splited-samples))])
         (make-kit-pict
          sample-rest
          new-line-at
          (vl-append last-line samples-pict)
          hidden?)))]))

(define/contract (make-kit-btmp kit [new-line-at 10] [hidden? #f])
  (->* (kit?) (positive? boolean?) (is-a?/c bitmap%))
  (pict->bitmap
   (make-kit-pict kit 10 (blank) hidden?)))