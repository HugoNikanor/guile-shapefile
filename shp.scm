;;; https://en.wikipedia.org/wiki/Shapefile
;;; https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf

(define-module (shp)
  :use-module (shp-header)
  :use-module (common)
  )

(import (rnrs (6)))

(use-modules (rnrs io ports)
             (rnrs bytevectors)
             ((srfi srfi-1)
              :select (iota split-at zip))
             (rnrs records syntactic)
             ;; (rnrs records inspection)
             ;; (rnrs records procedural)
             )

(module-export-all! (current-module))




;; (define-record-type null)

(define-record-type point
  (fields x y)) ; doubles
(create-printer point)

(define (parse-point bv offset)
  (apply make-point
         (map (lambda (i) (bytevector-ieee-double-ref bv i little))
              (iota 2 offset 8))))

(define-record-type multi-point
  (fields box ; rectangle
          points ; list of num-points <point>
          ))
(create-printer multi-point)

(define (parse-multi-point bv offset)
  (define box (parse-rectangle bv offset))
  (define point-count (bytevector-sint-ref bv (+ offset (* 8 4)) little 4))
  (define points
   (map (lambda (i) (parse-point bv i))
        (iota point-count (+ offset (* 8 4) 4) (* 8 2))))
  (make-multi-point box points))

(define-record-type poly-line
  (fields box ; bounding-box (<rectangle>)
          parts ; (list (list <point>))
          ))
(create-printer poly-line)

(define lll #f)

(define (parse-poly-* constructor data offset)
  (define box (parse-rectangle data offset))
  (define num-parts (bytevector-sint-ref data (+ offset (* 8 4))
                                         little offset))
  (define num-points (bytevector-sint-ref data (+ offset (* 9 4))
                                          little offset))
  (define part-indices
    (map (lambda (i) (bytevector-sint-ref data i little 4))
         (iota num-parts (+ offset (* 10 4)) 4)))

  (define points
    (map (lambda (i) (parse-point data i))
         (iota num-points (+ offset (* 10 4) (* 4 num-parts))
               (* 2 8))))

  (define parts
    (let ((lst (reverse part-indices)))
      (let loop ((rem points)
                 (take (map (lambda (args) (apply - args))
                            (zip (cons num-points lst) lst))))
        ;; (format #t "take=~s rem=~s ~s ~s~%"
        ;;         take rem (null? take) (null? rem))
        (set! lll rem)
        (if (null? rem)
            '()
            (call-with-values (lambda () (split-at rem (car take)))
              (lambda (head tail)
                ;; (format #t "head=~a tail=~a~%"
                ;;         head tail)
                (cons head (loop tail (cdr take)))))))))

  (constructor box parts))

(define (parse-poly-line data offset)
  (parse-poly-* make-poly-line data offset))

(define-record-type polygon
  (fields box ; <rectangle>
          ;; num-parts ; integer
          ;; num-points ; integer
          parts ; integer[num-parts]
          ;; points ; points[num-points]
          ))
(create-printer polygon)

(define (parse-polygon data offset)
  (parse-poly-* make-polygon data offset))

(define-record-type point-m
  (fields x y m) ; doubles
  )

(define-record-type multi-point-m
  (fields box ; rectangle
          num-points
          points ; point[num-points]
          m-range ; double, double
          m-array ; double[num-points]
          ))

(define-record-type poly-line-m
  (fields box ; rectangle
          num-parts
          num-points
          parts ; integer[num-parts]
          points ; point[num-points]
          m-range ; double, double
          m-array ; double[num-points]
          ))

(define-record-type polygon-m
  (fields box ; rectangle
          num-parts
          num-points
          parts ; integer[num-parts]
          points ; point[num-points]
          m-range ; double, double
          m-array ; double[num-points]
          ))



;; todo -z-types, multi-patch




(define parsers
  `((poly-line . ,parse-poly-line)
    (polygon . ,parse-polygon)
    (point . ,parse-point)
    (multi-point . ,parse-multi-point)
    )
  )


(define (get-parser symbol)
  (or (assoc-ref parsers symbol)
      (throw 'not-implemented
             "Parser ~a not yet implemented" symbol)))


(define (parse-shp-file port)
  (define header (parse-header (get-bytevector-n port 100)))

  (let loop ((remaining-length (- (shp-header-length header) 100)))
    (if (zero? remaining-length)
        '()
        (let* ((record-header-bytes (get-bytevector-n port 8))
               (record-number (bytevector-sint-ref
                               record-header-bytes 0 big 4))
               (record-length (* 2 (bytevector-sint-ref
                                    record-header-bytes 4 big 4)))
               (data (get-bytevector-n port record-length))
               (type (number->type (bytevector-sint-ref data 0 little 4))))

          (cond [(eq? type 'null)
                 (loop (- remaining-length 8 record-length))]
                [(eq? type (shp-header-type header))
                 (cons ((get-parser type) data 4)
                       (loop (- remaining-length 8 record-length)))]
                [else
                 (throw 'something)])))))


