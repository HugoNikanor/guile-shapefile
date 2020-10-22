;;; https://en.wikipedia.org/wiki/Shapefile
;;; https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf

(import (rnrs (6)))

(use-modules (rnrs io ports)
             (rnrs bytevectors)
             (srfi srfi-1)
             (rnrs records syntactic)
             (rnrs records inspection)
             (rnrs records procedural)
             ((srfi srfi-9 gnu) :select (set-record-type-printer!))
             )


(define big (endianness big))
(define little (endianness little))



(define (create-printer record)
  (set-record-type-printer!
   record
   (lambda (r p)
     (format p "#<~a" (record-type-name record))
     (let self ((record record))
       (when (record-type-parent record)
         (self (record-type-parent record)))
       (let ((v (record-type-field-names record)))
         (map
          (lambda (i)
            (format p " ~a=~a"
                    (vector-ref v i)
                    ((record-accessor record i) r)))
          (iota (vector-length v)))))
     (display ">" p))))



(define types
  '((null-shape . 0)
    (point . 1)
    (poly-line . 3)
    (polygon . 5)
    (multi-point . 8)
    (point-z . 11)
    (poly-line-z . 13)
    (polygon-z . 15)
    (multi-point-z . 18)
    (point-m . 21)
    (poly-line-m . 23)
    (polygon-m . 25)
    (multi-point-m . 28)
    (multi-patch . 31)))

(define (type->number name)
  (assoc-ref types name))

(define number->type
  (let ((v (make-vector 32)))
    (for-each (lambda (pair)
                (vector-set! v (cdr pair) (car pair)))
              types)
    (lambda (n)
      (vector-ref v n))))




(define-record-type rectangle
  (fields x-min x-max y-min y-max))
(create-printer rectangle)

(define (parse-rectangle bytevector offset)
  (apply make-rectangle
         (map (lambda (i) (bytevector-ieee-double-ref bytevector i little))
              (iota 4 offset 8))))

(define-record-type shp-header
  (fields file-code
          length
          version
          type
          bounding-box
          z-range                       ; z-min - z-max
          m-range))

(create-printer shp-header)

(define (parse-header bv)
  (define (bvref . args)
    (apply bytevector-sint-ref bv args))

  (make-shp-header
   (bvref 0 big 4)                       ; file code = 9994
   (* 2 (bvref 24 big 4))                ; file length (in bytes)
   (bvref 28 little 4)                   ; version = 1000
   (number->type (bvref 32 little 4))    ; shape-type
   (parse-rectangle bv 36)
   (map (lambda (i) (bytevector-ieee-double-ref bv i little))
        (iota 2 68 8))
   (map (lambda (i) (bytevector-ieee-double-ref bv i little))
        (iota 2 84 8))))








(define-record-type null)

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

(define (parse-poly-line data offset)
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
        (if (null? rem)
            '()
            (call-with-values (lambda () (split-at rem (car take)))
              (lambda (head tail)
                (cons head (loop tail (cdr take)))))))))

  (make-poly-line box parts))


(define-record-type polygon
  (fields box ; <rectangle>
          num-parts ; integer
          num-points ; integer
          parts ; integer[num-parts]
          points ; points[num-points]
          ))

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
  `((poly-line . ,parse-poly-line))
  )


(define (get-parser symbol)
  (assoc-ref parsers symbol))


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
               (data (get-bytevector-n shp record-length))
               (type (number->type (bytevector-sint-ref data 0 little 4))))

          (cond [(eq? type 'null)
                 (loop (- remaining-length 8 record-length))]
                [(eq? type (shp-header-type header))
                 (cons ((get-parser type) data 4)
                       (loop (- remaining-length 8 record-length)))]
                [else
                 (raise 'something)])))))

;; mostly here for completeness
(define (parse-shx-file port)
  (define header (parse-header (get-bytevector-n port 100)))
  (define data (get-bytevector-n port (- (shp-header-length header) 100)))
  (map (lambda (i)
         (cons (bytevector-sint-ref data       i big 4)
               (bytevector-sint-ref data (+ i 4) big 4)))
       (iota (/ (- (shp-header-length header) 100) 8)
             0 8)))

(define (main args)

  (define path "/home/hugo/code/lantmateriet/download-opendata.lantmateriet.se/GSD-Sverigekartor_vektor/1_milj/Sverige/Sweref_99_TM/shape/svk/riks")

  (define shp-file (string-append path "/vl_riks.shp"))
  (define shx-file (string-append path "/vl_riks.shx"))

  (define data (call-with-input-file shp-file parse-shp-file)))



