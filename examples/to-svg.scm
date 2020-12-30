#!/usr/bin/guile \
-e main -s
!#

(use-modules
 (shapefile)
 (shapefile shp)
 )




;; (list), (list (list)) → sxml fragment
(define (table heads body)
  `(table (thead (tr ,@(map (lambda (h) `(th ,h))
                            heads)))
          (tbody ,@(map (lambda (row)
                          `(tr ,@(map (lambda (item) `(td ,item))
                                      row)))
                        body))))


(define-syntax svg
  (syntax-rules (@)
    ((_ (@ params ...) body ...)
     ;; should preferably use assoc-merge here
     `(svg (@ (xmlns "http://www.w3.org/2000/svg")
              (version "1.1")
              params ... )
           (unquote body) ...))
    ((_ body ...)
     (svg (@) body ...))))




;; <shape>, int, <rectangle> → (list <point>) → sxml-fragment
(define (render-part shape side-length bounding-box)
  (define x-min (rectangle-x-min bounding-box))
  (define x-max (rectangle-x-max bounding-box))
  (define y-min (rectangle-y-min bounding-box))
  (define y-max (rectangle-y-max bounding-box))
  (define ratio
    (/ (- x-max x-min)
       (- y-max y-min)))

  (lambda (part)
    `(,(cond ((polygon? shape) 'polygon)
             ((poly-line? shape) 'polyline)
             (else (throw 'type-error)))
      (@ (fill ,(cond ((polygon? shape) "lightgreen")
                      ((poly-line? shape) "none")
                      (else (throw 'type-error))))
         (stroke "black")
         (stroke-width "1")
         (transform
          ,(format #f "scale(1, -1) translate(0, -~a)"
                   side-length))
         (points
          ,@(map
             (lambda (point)
               (format
                #f "~a,~a "
                (* side-length
                   ratio
                   (/ (- (point-x point) x-min)
                      (- x-max x-min)))
                (* side-length
                   (/ (- (point-y point) y-min)
                      (- y-max y-min)))))
             part))))))


(define* (render-poly-group
          records
          #:key
          (height 1000)
          (style '()))
  (define shp-data (map record-shape records))
  (define bounds (map get-bounding-box shp-data))
  (define x-min (apply min (map rectangle-x-min bounds)))
  (define x-max (apply max (map rectangle-x-max bounds)))
  (define y-min (apply min (map rectangle-y-min bounds)))
  (define y-max (apply max (map rectangle-y-max bounds)))
  (define bounding-box (make-rectangle x-min y-min
                                       x-max y-max))

 (define ratio
   (/ (- x-max x-min)
      (- y-max y-min)))

 (svg (@ (width ,(floor (* height ratio)) "px")
         (height ,height "px"))
      style
      `(g (@ (stroke "black") (stroke-width "1"))
          ,@(map (lambda (record)
                   (define shape (record-shape record))
                   `(g (@ (class "shape")
                          ,@(if (poly-line? shape)
                                '((fill "none"))
                                '((fill "lightgreen"))))
                       (text (@ (stroke-width 1) (fill "black")
                                (x 10) (y 20))
                             ,(assoc-ref (record-data record)
                                         'LANSNAMN))
                       ,@(map (render-part shape height bounding-box)
                              (get-parts shape))))
                 records))))



(use-modules (ice-9 getopt-long))

(define option-spec
  '((file (single-char #\f) (required? #t)
          (value #t))
    (outfile (single-char #\o) (value #t))
    (height (value #t))))

(define (main args)
  (define opts (getopt-long args option-spec))
  (define file (option-ref opts 'file #f))
  (define outfile
    (option-ref opts 'outfile
                (string-append (basename file) ".svg")))
  (define height
    (string->number (option-ref opts 'height "1000")))


  (when (string-contains (basename file) ".")
    (display "Please give filename without extension,\n")
    (display "since shapefiles are spread over multiple files.\n")
    (exit 1))

  (let ((shapefile (read-shapefile file)))
    (with-output-to-file outfile
      (lambda ()
        ((@ (sxml simple) sxml->xml)
         `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
                 ,(render-poly-group
                   (shapefile-records shapefile)
                   #:height height
                   #:style '(style "
.shape text {opacity: 0; pointer-events: none;}
.shape:hover text {opacity: 1;}
.shape:hover polygon {fill: red;}
.shape:hover polyline {stroke: red;}
"))))))))
