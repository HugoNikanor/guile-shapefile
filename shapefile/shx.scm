(define-module (shapefile shx)
  :use-module ((shapefile internal shp-header)
               :select (parse-header shp-header-length))
  :use-module (rnrs bytevectors)
  :use-module (rnrs io ports)
  :use-module ((srfi srfi-1) :select (iota))
  :export (parse-shx-file)
  )

;; mostly here for completeness
(define (parse-shx-file port)
  (define header (parse-header (get-bytevector-n port 100)))
  (define data (get-bytevector-n port (- (shp-header-length header) 100)))
  (map (lambda (i)
         (cons (bytevector-sint-ref data       i (endianness big) 4)
               (bytevector-sint-ref data (+ i 4) (endianness big) 4)))
       (iota (/ (- (shp-header-length header) 100) 8)
             0 8)))
