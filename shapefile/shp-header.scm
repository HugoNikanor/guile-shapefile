(define-module (shapefile shp-header)
  :use-module (shapefile common)
  :use-module (rnrs records syntactic)
  :use-module (rnrs bytevectors)
  :use-module ((srfi srfi-1) :select (iota))
  )

(module-export-all! (current-module))

(define-record-type rectangle
  (fields x-min y-min x-max y-max))
(create-printer rectangle)

(define (parse-rectangle bytevector offset)
  (apply make-rectangle
         (map (lambda (i) (bytevector-ieee-double-ref bytevector i little))
              (iota 4 offset 8))))

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

