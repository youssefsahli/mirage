#lang racket

(require racket/gui
         canvas-list
         "export.rkt"
         "import.rkt"
         "kits.rkt"
         pict)

(define kits (make-hash))

(define current-kit-name (make-parameter #f))
(define (get-current-kit) (hash-ref kits (current-kit-name) #f))
(define (make-current-kit-pict)
  (make-kit-pict (get-current-kit)))

(define (make-current-kit-btmp)
  (pict->bitmap (make-current-kit-pict)))

(define (set-current-kit kit [name #f])
  (when name (current-kit-name name))
  (hash-set! kits (current-kit-name) kit)
  (let ([I (map
            (λ (s) (make-sample-pict s 50 #t))
            kit)])
    (send CVL set-items I)))

(define F (new frame%
               [label "Mirage"]
               [width 512]
               [height 720]))

(define M
  (new menu-bar%
       [parent F]))

(define GK
  (new dialog%
       [label "Générer un kit"]
       [parent F]
       [style '(close-button)]))

(define GK:VC
  (new vertical-panel%
       [parent GK]
       [alignment '(left top)]))

(define GK:VC:name
  (new text-field%
       [label "Nom: "]
       [parent GK:VC]))

(define GK:VC:set-sample-number
  (let ([step 10])
  (new slider%
       [label "Nombre d'échantillons:"]
       [min-value 50]
       [max-value 200]
       [parent GK:VC]
       [horiz-margin 15]
       [style '(vertical-label horizontal)]
       [callback (λ (s ev)
                   (let* ([v (send s get-value)]
                          [new-value (* step (floor (/ v step)))]
                          [invalid-val (send GK:VC:set-invalid-number get-value)])
                     (when (< new-value invalid-val)
                       (send GK:VC:set-invalid-number set-value new-value))
                     (send s set-value new-value)))])))

(define GK:VC:set-invalid-number
  (let ([step 5])
  (new slider%
       [label "Dont invalides (positifs):"]
       [min-value 0]
       [max-value 50]
       [parent GK:VC]
       [horiz-margin 15]
       [style '(vertical-label horizontal)]
       [callback (λ (s ev)
                   (let ([v (send s get-value)])
                     (send s set-value
                           (* step (floor (/ v step))))))])))

(define GK:VC:GB
  (new group-box-panel%
       [label "Options"]
       [horiz-margin 15]
       [vert-margin 15]
       [parent GK:VC]))

(define GK:VC:set-opacity
  (new check-box%
       [label "Opaques:"]
       [parent GK:VC:GB]))

(define GK:VC:HC
  (new horizontal-panel%
       [parent GK:VC]
       [alignment '(center center)]))

(define GK:VC:gen-button
  (new button%
       [label "Générer"]
       [parent GK:VC:HC]
       [callback
        (λ (b ev)
          (let ([name (send GK:VC:name get-value)]
                [size (send GK:VC:set-sample-number get-value)]
                [invalids (send GK:VC:set-invalid-number get-value)]
                [opacity (send GK:VC:set-opacity get-value)])
            (let* ([K (make-kit size invalids opacity)])
              (current-kit-name name)
              (set-current-kit K))))]))

(define GK:VC:Cancel-button
  (new button%
       [label "Annuler"]
       [parent GK:VC:HC]
       [callback (λ (b ev) (send GK show #f))]))
  
(define M:kits
  (new menu%
       [label "&Kits"]
       [parent M]))

(define M:Affichage
  (new menu%
       [label "&Affichage"]
       [parent M]))

(define AO
  (new dialog%
       [label "Options d'Affichage"]
       [parent F]))

(define AO:reveal
  (new button%
       [label "Révéler les échantillons"]
       [parent AO]
       [callback (λ (b ev)
                   (when (get-current-kit)
                     (show-pict (make-current-kit-pict))))]))

(new menu-item%
     [label "Options"]
     [parent M:Affichage]
     [callback
      (λ (b ev) (send AO show #t))])

(new menu-item%
     [label "Shuffle"]
     [parent M:kits]
     [callback
      (λ (b ev)
        (when (get-current-kit)
          (set-current-kit (shuffle (get-current-kit)))))])

(new menu-item%
     [label "Générer un kit"]
     [parent M:kits]
     [callback (λ (b ev) (send GK show #t))])

(new menu-item%
     [label "Exporter image"]
     [parent M:kits]
     [callback (λ (b ev)
                 (if (not (get-current-kit))
                     (message-box "Aucun Kit" "Aucun kit n'a été créé." F)
                     (let ([path (put-file
                                  "choisir un nom"
                                  F
                                  (current-directory)
                                  "kitX.png"
                                  "*.png"
                                  null
                                  '(("PNG Image" "*.png")))])
                       (when path
                         (send (make-current-kit-btmp)
                               save-file path 'png))
                       )))])

(new menu-item%
     [label "Exporter en CSV"]
     [parent M:kits]
     [callback
      (λ (b ev)
        (if (not (get-current-kit))
            (message-box "Aucun Kit" "Aucun kit n'a été créé." F)
            (let ([path (put-file
                         "choisir un nom de fichier"
                         F
                         (current-directory)
                         "kitX.csv"
                         "*.csv"
                         null
                         '(("CSV File" "*.csv")))])
              (when path
                  (export-kit (get-current-kit) path)))))])

(new menu-item%
     [label "Charger un kit"]
     [parent M:kits]
     [callback
      (λ (b ev)
        (let ([path (get-file
                     "choisir un kit"
                     F
                     (current-directory)
                     "kitX.csv"
                     "*.csv"
                     null
                     '(("CSV File" "*.csv")))])
          (when path
            (let-values ([(base filename dir?) (split-path path)])
              (let ([retrieved-name (regexp-match
                                     #px"[KkIiTt]+[_]?([[:alnum:]]+)\\.csv"
                                     filename)])
                (when retrieved-name
                  (let ([kitname (second retrieved-name)]
                        [imported-kit (import-kit path)])
                    (set-current-kit imported-kit kitname))))))))])
                         
(define F:VP
  (new vertical-panel%
       [parent F]
       [stretchable-width #t]
       [stretchable-height #t]))

(define CVL
  (new canvas-list%
       [parent F:VP]
       [items '()]
       [item-height 64]
       [paint-item-callback
        (λ (cvl item state dc w h)
          (draw-pict item dc 0 0))]))
     
(send F show #t)