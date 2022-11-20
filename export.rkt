#lang racket

(provide export-kit)

(require csv-writing
         (only-in "kits.rkt" make-kit kit? sample))

(define/contract (export-kit kit path)
  (-> kit? path-string? void?)
  (call-with-output-file path
    (λ (out)
      (display
       (table->string
        (map
         (λ (s)
           (match s
             [(sample id op valid) (list id op valid)]))
         kit))
       out))
    #:exists 'replace))
