#lang racket

(provide import-kit)

(require csv-reading
         (only-in "kits.rkt" kit? sample))

(define/contract (string->boolean s)
  (-> string? boolean?)
  (not (string=? s "FALSE")))

(define/contract (import-kit path)
  (-> path-string? kit?)
  (call-with-input-file path
    (λ (in)
      (csv-map
       (λ (row)
         (match row
           [(list id opaque? valid?)
            (let ([idn (string->number id)]
                  [opb (string->boolean opaque?)]
                  [validb (string->boolean valid?)])
            (sample idn opb validb))]
           [else #f]))
       in))))